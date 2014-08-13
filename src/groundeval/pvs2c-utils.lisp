;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                PVS to C translator
;;
;;     Author: Gaspard ferey
;;
;;  -> https://github.com/Gaspi/pvs2c.git
;;  -> Please read  "main.lisp"
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;     Useful functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

;; --------------------------------------------------------------------
;;                 Ranges manipulations
;; --------------------------------------------------------------------

;; ------- Simple arithmetic on Z bar : { -, numbers, + } ---------
(defun <=zb (a b)
  (or (eql a '-)
      (eql b '+)
      (and (not (eql a '+))
	   (not (eql b '-))
	   (<= a b))))
(defun sign-zb (a)
  (if (eql a 0) 0 (if (<=zb a 0) -1 1)))
(defun min-zb (a b) (if (<=zb a b) a b))
(defun max-zb (a b) (if (<=zb a b) b a))
(defun add-zb (a b) (if (eql '- (min-zb a b)) '-
		    (if (eql '+ (max-zb a b)) '+ (+ a b))))
(defun neg-zb (a) (if (eql a '-) '+
		  (if (eql a '+) '- (- a))))
(defun sub-zb (a b) (add-zb a (neg-zb b)))
(defun mul-zb (a b)
  (let ((sign (* (sign-zb a) (sign-zb b))))
    (cond ((eql sign 0) 0)
	  ((not (and (numberp a)(numberp b)))
	   (if (> sign 0) '+ '-))
	  (t (* a b)))))


;; --------------- Classe to describe a range ----------------
(defcl C-range () (inf) (sup))
(defmethod C-range ((l list))
  (if (consp l)
      (make-instance 'C-range
		     :inf (if (and (car l) (not (eql (car l) '*)))
			      (car l) '-)
		     :sup (if (consp (cdr l))
			      (if (and (cadr l) (not (eql (cadr l) '*)))
				  (cadr l) '+)
			    (if (and (cdr l) (not (eql (cdr l) '*)))
				(cdr l) '+)))
    (make-instance 'C-range :inf '- :sup '+)))

;; Useful for debugging
(defmethod print-object ((obj C-range) out)
  (format out "[~a ; ~a]" (inf obj) (sup obj)))

;; ------------------- Basic manipulations ---------------------
(defun inter-range (ranges)
  (C-range (when (consp ranges)
	     (let ((tl (inter-range (cdr ranges))))
	       (cons (max-zb (inf (car ranges)) (inf tl))
		     (min-zb (sup (car ranges)) (sup tl)))))))

;; Returns a range containing the union of the given ranges
(defun union-range (ranges)
  (C-range (when (consp ranges)
	     (let ((tl (union-range (cdr ranges))))
	       (cons (min-zb (inf (car ranges)) (inf tl))
		     (max-zb (sup (car ranges)) (sup tl)))))))

;; Return a range containing the complementary of the given range
(defun compl-range (range)
  (with-slots (inf sup) range
  (C-range
   (cond ((and (eq inf '-) (eq sup '+))   ;; if range is all
	  (list 0 0))   ;; Singleton 0
	 ((eq inf '-)
	  (list (1+ sup)  '+))
	 ((eq sup '+)
	  (list '- (1- inf)))
	 (t nil)))))

(defun range-included (rangeA rangeB)
  (and (<=zb (inf rangeB) (inf rangeA))
       (<=zb (sup rangeA) (sup rangeB))))



;; --------------------------------------------------------------------
;;                 PVS ranges of types and expressions
;; --------------------------------------------------------------------

;; ---------- Is the given type the PVS integer type ? ----------
(defmethod PVS-int? ((type type-name))
  (PVS-int? (type (car (resolutions type)))))
(defmethod PVS-int? ((type subtype))
  (and (predicate type)
       (name-expr? (predicate type))
       (eq 'integer_pred (id (predicate type)))))
(defmethod PVS-int? (type) nil)

;; ----------------- Is it a subtype of it ? ---------------------
(defmethod subtype-PVS-int? ((type type-name))
  (subtype-PVS-int? (type (car (resolutions type)))))
(defmethod subtype-PVS-int? ((type type-application))
  (eql (id (type type)) 'mod))
(defmethod subtype-PVS-int? ((type null)) nil)
(defmethod subtype-PVS-int? (type)
  (or (PVS-int? type) (subtype-of? type *integer*)))

;; ------- C range computations ------------------
(defmethod C-range ((type type-name))
  (C-range (type (car (resolutions type)))))
(defmethod C-range ((type subtype))
  (if (or (not (subtype-PVS-int? type)) (PVS-int? type))
      (C-range nil)  ;; if this is bigger than an int (or not an int at all)
    (let* ((pred (predicate type))
	   (bind (car (bindings pred))) ;;bindings is a singleton
	   (decl-type (declared-type bind))
	   (id (id bind)))
      (when (and (not (null decl-type))
		 (not (subtype-PVS-int? decl-type)) (break)))
      (inter-range
       (list (C-range (supertype type))
	     (C-range decl-type)
	     (get-C-range (expression pred) id))))))


(defmethod get-C-range ((expr conjunction) id)
  (let ((args (arguments expr)))
    (inter-range (list (get-C-range (car args) id)
		       (get-C-range (cadr args) id)))))
(defmethod get-C-range ((expr disjunction) id)
  (let ((args (arguments expr)))
    (union-range (list (get-C-range (car args) id)
		       (get-C-range (cadr args) id)))))
;; This could actually give wrong results...
;; (defmethod get-C-range ((expr negation) id)
;;   (let ((args (arguments expr)))
;;     (compl-range (get-C-range (car args) id))))

(defmethod get-C-range ((expr infix-application) id)
  (let ((args (arguments expr))
	(oper (operator  expr)))
    (C-range
     (when (and (eql (length args) 2)
		(name-expr? oper))
       (let ((a1 (get-value (car  args)))
	     (a2 (get-value (cadr args)))
	     (op (id oper)))
	 (when (and (or (numberp a1) (numberp a2))   ;; one is a number
		    (or (eq id   a1) (eq id   a2)))  ;; and one is the id
	   (cond ((and (numberp a1) (eq op '<=))
		  (list a1 '+))
		 ((and (numberp a1) (eq op '<))
		  (list (1+ a1) '+))
		 ((and (numberp a1) (eq op '>=))
		  (list '- a1))
		 ((and (numberp a1) (eq op '>))
		  (list '- (1- a1)))
		 ((and (numberp a1) (eq op '=))
		  (list a1 a1))
		 ((and (numberp a2) (eq op '<=))
		  (list '- a2))
		 ((and (numberp a2) (eq op '<))
		  (list '- (1- a2)))
		 ((and (numberp a2) (eq op '>=))
		  (list a2 '+))
		 ((and (numberp a2) (eq op '>))
		  (list (1+ a2) '+))
		 ((and (numberp a2) (eq op '=))
		  (list a2 a2))
		 (t C-range nil))))))))

;; ------------- Get the value of an expression -------------	  
(defmethod get-value ((e name-expr))   (id e))
(defmethod get-value ((e number-expr)) (number e))
(defmethod get-value ((e unary-application))
  (when (eq (id (operator e)) '-)
    (let ((aux (get-value (argument e))))
      (when (numberp aux) (- aux)))))
(defmethod get-value (e) nil)

(defmethod get-C-range (expr id)
  (C-range nil))



(defmethod C-range ((type type-expr))
  (C-range (when (subtype-PVS-int? type)
	     (subrange-index type))))

(defmethod C-range ((expr number-expr))
  (C-range (cons (number expr) (number expr))))
;; Intersection of all bounds given by judgement-types
(defmethod C-range ((expr expr))
  (inter-range (mapcar #'C-range (get-PVS-types expr))))

(defmethod C-range ((expr application))
  (inter-range (list (C-range (type expr))
		     (C-range
   (when (pvs2cl-primitive? (operator expr))
     (let* ((op (pvs2C-primitive-op (operator expr)))
	    (args (mapcar #'C-range (arguments expr))))
       (cond ((negation-function? op args)
	      (cons (neg-zb (sup (car args)))
		    (neg-zb (inf (car args)))))
	     ((= (length args) 2)
	      (let ((i1 (inf (car  args)))
		    (s1 (sup (car  args)))
		    (i2 (inf (cadr args)))
		    (s2 (sup (cadr args))))
		(cond ((eql op 'pvsAdd)
		       (cons (add-zb i1 i2) (add-zb s1 s2)))
		      ((eql op 'pvsSub)
		       (cons (sub-zb i1 s2) (sub-zb s1 i2)))
		      ((eql op 'pvsTimes)
		       (cons (min-zb (min-zb (mul-zb i1 i2)
					     (mul-zb i1 s2))
				     (min-zb (mul-zb s1 i2)
					     (mul-zb s1 s2)))
			     (max-zb (max-zb (mul-zb i1 i2)
					     (mul-zb i1 s2))
				     (max-zb (mul-zb s1 i2)
					     (mul-zb s1 s2)))))
		      (t nil))))
	     (t nil))))))))



;; -------- Simple indentation function ---------
(defun indent (bloc)
  (if (listp bloc) (mapcar #'indent bloc)
    (format nil "  ~a" bloc)))


;; There is probably a Common Lisp function to do that...
(defun append-lists (l)
  (when (consp l) (append (car l) (append-lists (cdr l)))))

(defun range-arr (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))

(defun uli (&optional (str "~a"))
  (format nil str (if *Crename-uli* "uli" "unsigned long int")))


;; --------------------------------------------------------------------
;;                 Testing functions
;; --------------------------------------------------------------------

;; ------------------Debugging functions and variables ---------------
(defvar *Cshow-safe*         nil )
(defvar *Cshow-bang*         nil )
(defvar *Cshow-dupl*         nil )
(defvar *Cdebug*             nil )
(defvar *Csimple-names*      nil )
(defvar *Crename-uli*        nil )
(defvar *C-analysis*         t   )
(defvar *C-replace-analysis* t   )

(defun test-cases (tests &optional runnable)
  (when runnable (set-runnable-C))
  (loop for tst in tests
	do (progn (tc tst)
		  (format t "~2%Translating ~a..." tst)
		  (generate-C-for-pvs-file tst)
		  (format t "~%~a translated.~2%" tst)))
  (format t "All tests: done.~%"))

;; Set flags so that runnable C is generated
(defun set-runnable-C ()
  (setq *Cshow-safe*           nil )
  (setq *Cshow-bang*           nil )
  (setq *Cshow-dupl*           nil )
  (setq *Cdebug*               nil )
  (setq *Csimple-names*        nil )
  (setq *Crename-uli*          nil )
  (setq *C-analysis*           t   )
  (setq *C-replace-analysis*   t   ))

;; Sets flags so that debugable C is generated
(defun set-debugable-C ()
  (setq *Cshow-safe*           t   )
  (setq *Cshow-bang*           t   )
  (setq *Cshow-dupl*           t   )
  (setq *Cdebug*               t   )
  (setq *Csimple-names*        nil )
  (setq *Crename-uli*          t   )
  (setq *C-analysis*           t   )
  (setq *C-replace-analysis*   t   ))


;; --------------------------------------------------------------------
;;                 Draft / old functions
;; --------------------------------------------------------------------

(defun C_type (op)
  (let ((hashentry (gethash (declaration op) (C-hashtable))))
    (when hashentry (format nil "~a -> ~a"
	 (C-info-type-arg hashentry)
	 (C-info-type-out hashentry)))))

(defun C_definition (op)
  (let ((hashentry (gethash (declaration op) (C-hashtable))))
    (when hashentry (C-info-definition hashentry))))







(defmacro C-reset (array)
  `(setq ,array nil))
(defmacro C-save (array)
  `(setq ,array (list ,array)))
(defmacro C-load (array)
  `(if (listp (car ,array))
       (let ((res (cdr ,array)))
	 (setq ,array (car ,array))
	 res)
     (break)))
(defmacro C-add (array args)
  `(setq ,array (append ,array
	    (if (listp ,args) ,args (list ,args)))))
(defmacro C-flush (array)
  `(let ((res ,array))
     (setq ,array nil)
     res))




;; Instructions to allocate memory for new variables
(defvar *C-instructions* nil)
(defun reset-instructions () (C-reset *C-instructions*))
(defun add-instructions (instructions)
  (C-add *C-instructions* instructions))
(defun add-instruction (instruction)
  (C-add *C-instructions* (list instruction)))
(defun add-instructions-first (instructions)
  (setq *C-instructions* (append instructions *C-instructions*)))

;; Instructions to destruct previously allocated memory
(defvar *C-destructions* nil)
(defun reset-destructions () (C-reset *C-destructions*))
(defun add-destructions (destructions)
  (C-add *C-destructions* destructions))
(defun add-destruction (destruction)
  (C-add *C-destructions* (list destruction)))


;; -------------Debugging functions and variables ---------------
;; Not working well
(defun gen-name (id destr)
  (if *Csimple-names* (format nil "~a~:[~;_d~]" id destr)
    (gentemp (format nil "pvs_~a~:[~;_d~]" id destr))))

(defun debug (str) (when *Cdebug*) (format t "~%Debug: ~a" str))




;; (defun pvs2C-resolution-nondestructive (op-decl formals body range-type)
;;   (let* ((*destructive?* nil)
;; 	 (bind-ids (pvs2cl-make-bindings formals nil))
;; 	 (id-map (pairlis formals bind-ids))
;; 	 (C-type-out (pvs2C-type range-type))
;; 	 (return-void (C-gmp? C-type-out))
;; 	 (result-var (C-var C-type-out "result"))
;; 	 (C-args (loop for var in formals
;; 		       collect (C-var (pvs2C-type (type var))
;; 				      (cdr (assoc var id-map)))))
;; 	 (C-type-arg (append (when return-void (list (C-var C-type-out "result")))
;; 			     C-args))
;; 	 (hash-entry (gethash op-decl *C-nondestructive-hash*))
;; 	 (C-body (pvs2C2 body id-map nil result-var (not return-void))))
;;              ;; If we don't return void, we need to malloc the result
;;     (debug (format nil "Defining (nondestructively) ~a with type~%   ~a -> ~a"
;; 		   (id op-decl) (mapcar #'type C-type-arg) C-type-out))
;;     (when *eval-verbose* (format t "~%as :~%~{~a~%~}" (instr C-body)))
;;     (setf (C-info-type-out hash-entry) (if return-void "void" C-type-out)
;; 	  (C-info-type-arg hash-entry) C-type-arg
;; 	  (C-info-C-code   hash-entry) C-body
;; 	  (C-info-definition hash-entry)
;; 	  (Cfun-decl (append (instr C-body)
;; 			     (if return-void (destr C-body) (list (Creturn result-var))))
;; 		     C-args nil))))

;; (defun pvs2C-resolution-destructive (op-decl formals body range-type)
;;   (let* ((*destructive?* t)
;; 	 (bind-ids (pvs2cl-make-bindings formals nil))
;; 	 (id-map (pairlis formals bind-ids))
;; 	 (C-type-out (pvs2C-type range-type))
;; 	 (return-void (C-gmp? C-type-out))
;; 	 (result-var (C-var C-type-out "result"))
;; 	 (C-args (loop for var in formals
;; 		       collect (C-var (pvs2C-type (type var))
;; 				      (cdr (assoc var id-map)))))
;; 	 (C-type-arg (append (when return-void (list (C-var C-type-out "result")))
;; 			     C-args))
;; 	 (hash-entry (gethash op-decl *C-destructive-hash*))
;; 	 (old-output-vars (C-info-analysis hash-entry))
;; 	 (C-body (pvs2C2 body id-map nil result-var (not return-void))))
;;              ;; If we don't return void, we need to malloc the result
;;     (debug (format nil "Defining (destructively) ~a with type~%   ~a -> ~a"
;; 		   (id op-decl) (mapcar #'type C-type-arg) C-type-out))
;;     (when *eval-verbose* (format t "~%as :~%~{~a~%~}" (instr C-body)))
;;     (setf (C-info-type-out hash-entry) (if return-void "void" C-type-out)
;; 	  (C-info-type-arg hash-entry) C-type-arg
;; 	  (C-info-C-code   hash-entry) C-body
;; 	  (C-info-definition hash-entry)
;; 	  (Cfun-decl (append (instr C-body)
;; 			     (if return-void (destr C-body) (list (Creturn result-var))))
;; 		     C-args t))))





;; Useless functions ?/

;; (defun pvs2C-assign-rhs (assignments bindings livevars)
;;   (when (consp assignments)
;;     (let* ((e (expression (car assignments)))
;; 	   (C-assign-expr (pvs2C e
;; 				 bindings
;; 				 (append (updateable-vars
;; 					  (arguments (car assignments)))
;; 					 (append (updateable-vars (cdr assignments))
;; 						 livevars))))
;; 	   (*lhs-args* nil))
;;       (cons C-assign-expr
;; 	    (pvs2C-assign-rhs (cdr assignments) bindings
;; 			      (append (updateable-free-formal-vars e)
;; 				      livevars))))))



