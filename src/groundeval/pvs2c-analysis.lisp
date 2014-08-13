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
;; - Definition of the function declaration (Cfun-decl) class.
;; - Analysis and optimization of the C code (updates)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

;; --------------------------------------------------------------------
;;             Useful functions mapping on C code
;; --------------------------------------------------------------------

;; ------- Maps f to all expressions and returns the concatenation of all results --------
(defun map-Ccode (e f &optional (ignore #'(lambda (x) nil)))
  (unless (funcall ignore e) (map-Ccode* e f ignore)))
(defmethod map-Ccode* ((l list) f ig)
  (when (consp l) (append (map-Ccode (car l) f ig) (map-Ccode (cdr l) f ig))))
(defmethod map-Ccode* ((v C-var)       f ig) (funcall f v))
(defmethod map-Ccode* ((e Crecord-get) f ig) (append (funcall f e) (map-Ccode (var e) f ig)))
(defmethod map-Ccode* ((e Carray-get)  f ig) (append (funcall f e) (map-Ccode (list (var e) (arg e)) f ig)))
(defmethod map-Ccode* ((e Cfuncall)    f ig) (append (funcall f e) (map-Ccode (args e) f ig)))
(defmethod map-Ccode* ((i Cfuncall-mp) f ig) (append (funcall f i) (map-Ccode (Cfunc i) f ig)))
(defmethod map-Ccode* ((i Cdecl)       f ig) (append (funcall f i) (map-Ccode (var i) f ig)))
;; (defmethod map-Ccode* ((i Cfree)    f ig) (append (funcall f i) (map-Ccode (var i) f ig)))
(defmethod map-Ccode* ((i Cset)        f ig) (append (funcall f i) (map-Ccode (list (var i)  (expr i)) f ig)))
(defmethod map-Ccode* ((i Ccopy)       f ig) (append (funcall f i) (map-Ccode (list (varA i) (varB i)) f ig)))
(defmethod map-Ccode* ((i Carray-init) f ig) (append (funcall f i) (map-Ccode (list (var i)  (body i)) f ig)))
(defmethod map-Ccode* ((i Crecord-init) f ig)(append (funcall f i) (map-Ccode (var i) f ig)))
(defmethod map-Ccode* ((i Creturn)     f ig) (append (funcall f i) (map-Ccode (var i) f ig)))
(defmethod map-Ccode* ((i Cif) f ig)
  (append (funcall f i)  (map-Ccode (list (cond-part i) (then-part i) (else-part i)) f ig)))
(defmethod map-Ccode* ((e Ccode) f ig) nil)
(defmethod map-Ccode* (e f ig) (break "Mapvars encountered unknown C expression..."))

;; ------------------- Maps f only to instructions ----------------------
(defun map-Cinstr (body f)
  (map-Ccode body f #'(lambda (x) (not (Cinstr? x)))))

;; --------------- Maps update function f to all C expressions ----------------
(defmethod upd-Ccode ((v C-var) f) (funcall f v))
(defmethod upd-Ccode ((l list) f)
  (when (consp l)
    (let ((hd (upd-Ccode (car l) f))
	  (tl (upd-Ccode (cdr l) f)))
      (if hd (cons hd tl) tl))))
(defmethod upd-Ccode ((e Carray-get) f)
  (setf (var  e) (upd-Ccode (var  e) f))
  (setf (arg  e) (upd-Ccode (arg  e) f))
  (funcall f e))
(defmethod upd-Ccode ((e Crecord-get) f) (setf (var  e) (upd-Ccode (var  e) f)) (funcall f e))
(defmethod upd-Ccode ((e Cfuncall) f)    (setf (args e) (upd-Ccode (args e) f)) (funcall f e))
(defmethod upd-Ccode ((i Cfuncall-mp) f) (setf (Cfunc i)(upd-Ccode (Cfunc i)f)) (funcall f i))
(defmethod upd-Ccode ((i Cdecl) f)       (setf (var  i) (upd-Ccode (var  i) f)) (funcall f i))
(defmethod upd-Ccode ((i Cfree) f)       (setf (var  i) (upd-Ccode (var  i) f)) (funcall f i))
(defmethod upd-Ccode ((i Cset) f)
  (setf (var  i) (upd-Ccode (var  i) f))
  (setf (expr i) (upd-Ccode (expr i) f))
  (funcall f i))
(defmethod upd-Ccode ((i Ccopy) f)
  (setf (varA i) (upd-Ccode (varA i) f))
  (setf (varB i) (upd-Ccode (varB i) f))
  (funcall f i))
(defmethod upd-Ccode ((i Carray-init) f)
  (setf (var  i) (upd-Ccode (var  i) f))
  (setf (body i) (upd-Ccode (body i) f))
  (funcall f i))
(defmethod upd-Ccode ((i Crecord-init) f) (setf (var i) (upd-Ccode (var i) f)) (funcall f i))
(defmethod upd-Ccode ((i Creturn) f)      (setf (var i) (upd-Ccode (var i) f)) (funcall f i))
(defmethod upd-Ccode ((i Cif) f)
  (setf (cond-part i) (upd-Ccode (cond-part i) f))
  (setf (then-part i) (upd-Ccode (then-part i) f))
  (setf (else-part i) (upd-Ccode (else-part i) f))
  (funcall f i))
(defmethod upd-Ccode (e f) (break "Mapvars encountered unknown C expression..."))

;;  ------------------ Maps f only to instructions ----------------------
(defun upd-Cinstr (body f)
  (upd-Ccode body #'(lambda (x) (if (Cinstr? x) (funcall f x) x))))



;; --------------------------------------------------------------------
;;                 C function declarations
;; --------------------------------------------------------------------

(defcl Cfun-decl () (id) (type-out) (args) (all-vars) (flags) (body) (bang-return?))
(defun Cfun-decl (id type-out body args)
  (make-instance 'Cfun-decl
		 :id id :type-out type-out :bang-return? t
		 :args args
		 :flags (init-flags args)
		 :all-vars (get-set (get-all-vars (list args body)))
		 :body body))

;; ----- Return all pointer arguments in f -----------
(defmethod pointer-args ((f Cfun-decl))
  (pointer-args (args f)))
(defmethod pointer-args ((l list))
  (when (consp l)
    (let ((hd (car l))
	  (tl (pointer-args (cdr l))))
      (if (pointer? hd)
	  (cons hd tl) tl))))

;; -------- Printing a function declaration -------------
(defun print-signature (f out)
  (assert (Cfun-decl? f))
  (let ((*Ccurr-f* f))
    (format out "~2%~a ~a(~a);" (type-out f) (id f) (sign-var (args f)))))
(defun print-definition (f out)
  (assert (Cfun-decl? f))
  (let ((*Ccurr-f* f))
    (format out  "~2%~:[~;!~]~a ~a(~a) {~%~{~a~%~}}"
	    (and *Cshow-bang* (bang-return? f))
	    (type-out f)
	    (id f)
	    (sign-var (args f))
	    (get-C-instructions f))))
(defmethod print-object ((obj Cfun-decl) out)
  (print-definition obj out))
(defmethod get-C-instructions ((f Cfun-decl))
  (indent (get-C-instructions (body f))))






;; --------------------------------------------------------------------
;;                 Variables and flags management
;; --------------------------------------------------------------------

;; ------- Current function declaration beeing analysed --------
(defvar *Ccurr-f* nil)

;; ----- Is Cvar an argument or a local variable in the current function ? ----
(defun is-arg? (Cvar &optional (f *Ccurr-f*))
  (member Cvar (args f) :test #'eq-C-var))
(defun is-local? (Cvar &optional (f *Ccurr-f*))
  (not (is-arg? Cvar f)))

;; ---------------- Return variable and type --------------
(defun get-return-vars (f)
  (map-Ccode (body f) #'(lambda (x) (when (Creturn? x) (list (var x))))))


(defun update-bang-return (f)
  (let ((new-br (null (loop for e in (get-return-vars f)
			    when (not (bang? e))
			    collect nil))))
(when (not (eql (bang-return? f) new-br))
  (setf (bang-return? f) new-br)
  t)))
(defmethod bang-return? ((f const-decl))
  (bang-return? (C-info-definition (gethash f (C-hashtable)))))
(defmethod bang-return? ((f null)) nil)  ;; allows to call (bang-return? nil)


;; ------------ Class to represent the flags state of a variable ----------
(defcl Cflags () (bang) (dupl) (arg) (treated))
(defun Cflags (&optional bang arg)
  (make-instance 'Cflags :bang bang :dupl nil :arg arg :treated arg))
(defmethod bang    (Cvar) nil)
(defmethod dupl    (Cvar) nil)
(defmethod treated (Cvar) nil)

;; --- Initializes the flags assoc list with the arguments of a function --
(defun init-flags (args)
  (when (consp args)
    (let ((hd (car args))
	  (tl (init-flags (cdr args))))
      (if (pointer? hd)
	  (acons (name hd) (Cflags *destructive?* t) tl)
	tl))))

;; ---------- Return the Cflags associated to the Cvar ----------
(defun get-flags (Cvar &optional (f *Ccurr-f*))
  (when (and f (pointer? Cvar))
    (let ((e (assoc Cvar (flags f) :test #'eq-C-var)))
      (if e (cdr e)
	(let ((new-e (Cflags)))  ;; Create a default Cflags
	  (setf (flags f) (acons (name Cvar) new-e (flags f)))
	  new-e)))))


;; ------- Return nil if the variable is currently considered not bang ---
(defmethod bang? ((fc Cfuncall) &optional (f *Ccurr-f*))
  (bang-return? (get-definition fc)))
(defmethod bang? (Cvar &optional (f *Ccurr-f*)) (bang (get-flags Cvar f)))


;; --------- Is it legal to flag this var ? (local & not treated yet) ---------
(defmethod add-bang? ((e Cflags)) (not (or (arg e) (treated e))))
(defmethod add-bang? (Cvar) (when Cvar (add-bang? (get-flags Cvar))))
(defmethod add-bang ((e Cflags))
  (when (add-bang? e)
    (setf (bang    e) t)
    (setf (treated e) t)))  ;; This flag guarantees variables are never flagged bang twice
(defmethod add-bang (Cvar) (when Cvar (add-bang (get-flags Cvar))))

;; --------- Is it legal to unflag this var ? ---------
(defmethod rem-bang? ((e Cflags)) (bang e))
(defmethod rem-bang? (Cvar)       (when Cvar (rem-bang? (get-flags Cvar))))
;; ------ Unflags the var(s). Return nil if no removal was legal ----------
(defmethod rem-bang ((e Cflags))  (when (rem-bang? e) (setf (bang e) nil) t))
(defmethod rem-bang ((l list))
  (when (consp l)
    (let ((hd (rem-bang (car l)))
	  (tl (rem-bang (cdr l))))
      (or hd tl))))
(defmethod rem-bang (Cvar) (rem-bang (get-flags Cvar)))


;; -------- Is a variable or a simple expresion dupl ? ------------
(defmethod dupl? ((e Crecord-get) &optional (f *Ccurr-f*)) (dupl? (var e) f))
(defmethod dupl? ((e Carray-get)  &optional (f *Ccurr-f*)) (dupl? (var e) f))
(defmethod dupl? (Cvar            &optional (f *Ccurr-f*))
  (when Cvar (dupl (get-flags Cvar f))))

;; ----------- The dupl flag can't be removed ----------------------
(defmethod set-dupl ((e Cflags))
  (when (not (dupl e)) (setf (dupl e) t)))
(defmethod set-dupl ((e Carray-get))  (set-dupl (var e)))
(defmethod set-dupl ((e Crecord-get)) (set-dupl (var e)))
(defmethod set-dupl ((e Cfuncall))    (set-dupl (get-dupl-args e)))
(defmethod set-dupl ((l list))
  (when (consp l)
    (let ((hd (set-dupl (car l)))
	  (tl (set-dupl (cdr l))))
      (or hd tl))))
(defmethod set-dupl (Cvar) (set-dupl (get-flags Cvar)))





;; --------------------------------------------------------------------
;;                 C code analysis
;; --------------------------------------------------------------------

;; ------- Operations on sets of variables ---------------
(defun unionvar (x y) (union x y :test #'eq-C-var))  
(defun get-set (l)
  (if (consp l)
      (unionvar (get-set (car l)) (get-set (cdr l)))
    (when l (list l))))

;; --------------- Gets all pointer-type variables -----------------
(defun get-all-vars (body)
  (map-Ccode body #'(lambda (x) (when (pointer? x) (list x)))))



;; ---------- Replace all pointer-type variables according to assoc list ----------
(defun replace-variables (body alist)
  (upd-Ccode body #'(lambda (x) (or (cdr (assoc x alist :test #'eq-C-var)) x))))

;; ----- The main analysis loop. The recursive call should terminate... ------------
(defun C-analysis (op-decl)
  (let ((f  (C-info-definition (gethash op-decl *C-nondestructive-hash*)))
	(fd (C-info-definition (gethash op-decl *C-destructive-hash*))))
    (and (or (C-analysis* f)
    	     (C-analysis* fd)      ;; if both return nil  (no change)
	     (C-last-analysis f)   ;; perform a last analysis on both
	     (C-last-analysis fd))
    	 (C-analysis op-decl))))  ;; else try again


;; ------------------ Here is the body of the analysis ---------------
(defun C-analysis* (f)
  (with-slots (body) f
  (let ((*Ccurr-f* f))
    (unsafe-all-pointers body)
    (init-analysis f)
    ;; (break)
    (when *C-replace-analysis*
      (or
	  (C-up-analysis            f)
	  (C-down-analysis          f)
          (C-analysis-destr-funcall body)
	  (C-analysis-dupl-funcall  body)
	  (C-analysis-replace-rule  f)
;;	  (break "new-notbang")
	  (C-analysis-new-notbang   body)
;;	  (break "new-bang") 
	  (C-analysis-new-bang      body)
;;	  (break "dupl")
	  (C-analysis-dupl body)
;;	  (break "set-to-copy")
	  (C-analysis-set-to-copy   body)
;;	  (break "bang-return")
	  (update-bang-return       f) ;;Update the return-bang? flag
	  )))))



;; ------ This function unflags "safe" in all occurence of variables -----
(defun unsafe-all-pointers (l)
  (upd-Ccode l #'(lambda (x) (if (and (pointer? x) (safe x))
				 (C-var (type x) (name x) nil) x))))

;; --------- This function flags "safe" all last occurence of a variable -----------
(defun safety-analysis (df)
  (safety-analysis* (reverse (body df)) nil))

(defmethod safety-analysis* ((l list) safe)
  (if (null l) safe
    (safety-analysis* (cdr l) (safety-analysis* (car l) safe))))

(defmethod safety-analysis* ((i Cif) safe)
  (safety-analysis* (cond-part i)
		    (unionvar (safety-analysis* (reverse (then-part i)) safe)
			      (safety-analysis* (reverse (else-part i)) safe))))

(defmethod safety-analysis* ((i Ccode) safe)
  (let* ((vars (get-all-vars i))
	 (candidates (set-difference vars safe :test #'eq-C-var))
	 (set-cand   (get-set candidates)))
    (replace-variables i (loop for c in set-cand
			       when (= (count c candidates :test #'eq-C-var) 1)
			       collect (cons c (safe-var c))))
    (unionvar set-cand safe)))


(defun clean-code (l)
  (when (consp l)
    (let ((c (car l)))
      (when (Cif? c)
	(setf (then-part c) (clean-code (then-part c)))
	(setf (else-part c) (clean-code (else-part c))))
      (if (get-C-instructions c)
	  (cons c (clean-code (cdr l)))
	(clean-code (cdr l))))))

;; --------- This function put all safe flags, unbang safe (unused) arguments,
;; --------- and flag the return variable as dupl
(defun init-analysis (f)
  (assert (Cfun-decl? f))
  (setf (body f) (clean-code (body f)))
  (let* ((used   (safety-analysis f))
	 (unused (set-difference (pointer-args f) used :test #'eq-C-var)))
    (rem-bang unused)
    (setf (args f)
	  (mapcar #'(lambda (x) (if (member x unused :test #'eq-C-var) (safe-var x) x))
		  (args f)))
    (set-dupl (get-return-vars f))))  ;; Init the analysis of dupl vars


;; A replacement rule  (varA <= varB)  is treated by:
;;  - Deleting the decl and init of name
;   - Deleting the current set or copy instruction
;;  - Deleting the free of var
;;  - Replacing occurences of name by var
;;  - Unsafing all variables !!!
;;  - Redo a safe analysis

;; ------ Analysis to find variables that can be replaced by an other -----------
(defun C-analysis-replace-rule (f)
  (let ((bodyp (apply-replacements (body f))))
    (setf (body f) (cdr bodyp))
    (car bodyp)))

(defun apply-replacements (body)
  (let ((rr (get-replace-rule body)))
    (if (consp rr)
	(cons t (upd-Cinstr body
			    #'(lambda (x)
				(unless (eq x (car rr))
				  (if (Cdecl? x) x
				    (replace-variables x (list (cdr rr))))))))
      (cons rr body))))

(defmethod get-replace-rule ((i list))
  (when (consp i) (or (get-replace-rule (car i))
		      (get-replace-rule (cdr i)))))
(defmethod get-replace-rule ((i Cinstr))
  (cond ((and (Cset? i)
	      (C-var? (var i))
	      (C-var? (expr i))
	      (type= (type (var i)) (type (expr i)))
	      (safe (expr i)))
	 (list* i (var i) (safe-var (expr i) nil)))
	((and (Ccopy? i)
	      (C-var? (varA i))
	      (C-var? (varB i))
	      (bang?  (varB i))
	      (safe   (varB i)))
	 (list* i (varA i) (safe-var (varB i) nil)))
	((Cif? i)
	 (let ((thenp (apply-replacements (then-part i)))
	       (elsep (apply-replacements (else-part i))))
	   (setf (then-part i) (cdr thenp))
	   (setf (else-part i) (cdr elsep))
	   (or (car thenp) (car elsep))))
	(t nil)))





;; --------- Analysis to remove bang flags ---------------
(defun C-analysis-new-notbang (body)
  (let ((new-nb (map-Ccode body #'get-new-bang-unflag)))
    (rem-bang new-nb)))
(defmethod get-new-bang-unflag ((e Carray-get))
  (when (and (bang? (var e)) (safe (var e))) (list (var e))))
(defmethod get-new-bang-unflag ((e Crecord-get))
  (when (and (bang? (var e)) (safe (var e))) (list (var e))))
(defmethod get-new-bang-unflag (other) nil)



;; --------- Analysis to add new bang flags ---------------
(defun C-analysis-new-bang (body)
  (let ((new-b (car (get-new-bang-flag body))))
    (when (and new-b (add-bang? new-b))
      (add-bang new-b)
      t)))

;; These return a list of potential bang variables
(defmethod get-new-bang-flag ((i list))
  (when (consp i) (append (get-new-bang-flag (car i))
			  (get-new-bang-flag (cdr i)))))
(defmethod get-new-bang-flag ((i Ccopy))
  (let ((var (varA i))) (when (add-bang? var) (list var))))
(defmethod get-new-bang-flag ((i Carray-init))
  (let ((var (var i)))  (when (add-bang? var) (list var))))
(defmethod get-new-bang-flag ((i Crecord-init))
  (let ((var (var i)))  (when (add-bang? var) (list var))))
(defmethod get-new-bang-flag ((i Cset))
  (let ((e (expr i)))
    (when (and (safe e) (bang? e) (add-bang? (var i))) (list (var i)))))
(defmethod get-new-bang-flag ((i Cif))
  (unionvar (get-new-bang-flag (then-part i))
	    (get-new-bang-flag (else-part i))))
(defmethod get-new-bang-flag ((i Cinstr)) nil)



;; ------ Quick analysis to replace bang-unsafe Cset to Ccopy ---------------
;; ----- To be done at the very end for the least number of new copies ------
(defun C-analysis-set-to-copy (body)
  (null ;; This analysis doesn't require a restart of all analysis
   (upd-Cinstr body #'(lambda (x)
		       (if (and (Cset? x)
				(bang? (expr x))   ;; This means (expr x) is a C-var
				(not (safe (expr x))))
			   (Ccopy (var x) (expr x))
			 x)))))

;; ------ Analysis to propagate dupl flags -------
(defun C-analysis-dupl (body)
  (map-Cinstr (reverse body) #'C-analysis-dupl*))

(defmethod C-analysis-dupl* ((i Cset))
  (when (dupl? (var i))
    (and (set-dupl (expr i)) (list t))))
(defmethod C-analysis-dupl* ((i Cinstr)) nil)





;; We try to change a funcall non-destr (default) into a funcall destr
;; Conditions :
;;  - Arguments bang are variables safe and bang
;;  - Arguments dupl are variables safe or not bang
;; Consequences:
;;  -> flag destr in funcall set to t
;;  -> if result is dupl, all variables passed as dupl arguments are flagged dupl

;; ---- Gets all arguments expressions corresponding to arguments flagged bang -----
(defun get-bang-args (fc &optional destr)
  (assert (Cfuncall? fc))
  (let ((def (if destr (get-definition fc t)
	               (get-definition fc))))
    (when def (get-bang-args* (args fc) (args def) def))))
(defun get-bang-args* (l names def)
  (when (consp l)
    (let ((tl (get-bang-args* (cdr l) (cdr names) def)))
      (if (bang? (car names) def)
	  (cons (car l) tl)
	tl))))

;; ---- Gets all arguments expressions corresponding to arguments flagged dupl -----
(defun get-dupl-args (fc)
  (assert (Cfuncall? fc))
  (let ((def (get-definition fc)))
    (when def (get-dupl-args* (args fc) (args def) def))))
(defun get-dupl-args* (l names def)
  (when (consp l)
    (let ((tl (get-dupl-args* (cdr l) (cdr names) def)))
      (if (dupl? (car names) def) (cons (car l) tl) tl))))


(defun C-analysis-destr-funcall (body)
  (map-Ccode body #'destr-funcall))
(defun destr-funcall (fc)
  (when (and (Cfuncall? fc)               ;; fc is a call...
	     (const-decl? (name (fun fc)));; ... to a PVS function ...
	     (not (destr (fun fc)))      ;; ... non destructive version
	     (all-safe-and-bang (get-bang-args fc t)))
;;    (break)
    (setf (destr (fun fc)) t)
    (list t)))

(defun all-safe-and-bang (l)
  (or (null l)
      (and (safe  (car l))
	   (bang? (car l))
	   (all-safe-and-bang (cdr l)))))


(defun C-analysis-dupl-funcall (body)
  (map-Ccode body #'dupl-funcall))
(defun dupl-funcall (fc)
  (when (and (Cfuncall? fc)
	     (get-definition fc))    ;; useless ?
    (let ((rb
	   (loop for e in (get-dupl-args fc)
		 when (and (bang? e) (not (safe e)) (C-var? e))
		 collect e)))
      (when (rem-bang rb)
	(list t)))))



;; Last modifications to optimize the code
;; set(res, ??)   => return ??
;; return res;
;;
;; if () {  res = ?1? }  =>  if () { return ?1? }
;; else  {  res = ?2? }  =>  else  { return ?2? )
;; return res;
;; 
(defun C-up-analysis (f)
  (setf (body f) (reverse (C-up-free-analysis (reverse (body f)) nil)))
  (let* ((bodyp (C-up-return-analysis (body f))))
    (setf (body f) (cdr bodyp))
    (car bodyp)))

;; Returns (cons t/nil new-list)
(defun C-up-return-analysis (l)
  (if (< (length l) 2) (cons nil l)
    (let* ((lasts (last l 2))
	   (last  (cadr lasts))
	   (2last (car  lasts)))
      (cond ((and (Creturn? last) (Cif? 2last))
	     (setf (then-part 2last)
		   (append (then-part 2last) (list (Creturn (C-var-copy (var last))))))
	     (setf (else-part 2last)
		   (append (else-part 2last) (list (Creturn (C-var-copy (var last))))))
	     (cons t (butlast l)))
	    ((and (Creturn? last) (Cset? 2last) (C-var? (var 2last)) (C-var? (var last))
		  (eq-C-var (var last) (var 2last)))
	     (setf (var last) (expr 2last))
	     (cons t
		   (append (upd-Cinstr (butlast l 2)
				       #'(lambda (x) (unless (and (Cdecl? x)
								  (eq-C-var (var x) (var 2last)))
						       x)))
			   (last l))))
	    ((Cif? last)
	     (let ((thenp (C-up-return-analysis (then-part last)))
		   (elsep (C-up-return-analysis (else-part last))))
	       (setf (then-part last) (cdr thenp))
	       (setf (else-part last) (cdr elsep))
	       (cons (or (car thenp) (car elsep)) l)))
	    (t (cons nil l))))))

;; Return a list of the pointer variable if it is READ (not freed)
;; Only safe vars passed as argument are freed (unsafe vars are GCed)
(defmethod get-read-vars ((e Carray-get))
  (when (and (C-var? (var e)) (pointer? (var e))) (list (var e))))
(defmethod get-read-vars ((e Crecord-get))
  (when (and (C-var? (var e)) (pointer? (var e))) (list (var e))))
(defmethod get-read-vars ((e C-var))
  (when (not (safe e)) (list e)))
(defmethod get-read-vars ((e Ccode)) nil)

(defmethod C-up-free-analysis ((l list) tofree)
  (if (null l) (loop for v in tofree collect (Cfree v))
    (let ((i (car l)))
      (if (and (Cfree? i) (pointer? (var i)))
	  (C-up-free-analysis (cdr l) (unionvar (list (var i)) tofree))
	(if (Cif? i)
	    (let ((tofree-if (set-difference tofree
					     (get-all-vars (cdr l)) :test #'eq-C-var)))
	      (setf (then-part i) (reverse (C-up-free-analysis (reverse (then-part i))
							       tofree-if)))
	      (setf (else-part i) (reverse (C-up-free-analysis (reverse (else-part i))
							       tofree-if)))
	      (cons i (C-up-free-analysis (cdr l) (set-difference tofree
								  tofree-if
								  :test #'eq-C-var))))
	  (append (loop for v in (intersection tofree (get-set (map-Ccode i #'get-read-vars))
					       :test #'eq-C-var)
			collect (Cfree v))
		  (cons i
			(C-up-free-analysis (cdr l)
					    (set-difference tofree (get-all-vars i)
							    :test #'eq-C-var)))))))))



(defun get-vars-instr (body)
  (get-set (map-Ccode body #'(lambda (x) (when (C-var? x) (list x))))))

(defun C-down-analysis (f)
  (setf (body f) (C-down-decl-analysis (body f) nil))
  nil)

(defmethod C-down-decl-analysis ((l list) todecl)
  (when (consp l)
    (let ((i (car l)))
      (cond ((Cdecl? i)  ;;  (and (Cdecl? i) (pointer? (var i)))
	     (C-down-decl-analysis (cdr l) (unionvar (list (var i)) todecl)))
	    ((and (Cfree? i) (member (var i) todecl :test #'eq-C-var))
	     (C-down-decl-analysis (cdr l) (set-difference todecl (list (var i))
							   :test #'eq-C-var)))
	    ((Creturn? i)
	     (append (loop for v in todecl collect (Cdecl v)) (list i)))
	    (t (let ((todecl-now   (intersection   todecl (get-vars-instr i) :test #'eq-C-var)))
		 (when (Cif? i)
		   (setf (then-part i) (C-down-decl-analysis (then-part i) nil))
		   (setf (else-part i) (C-down-decl-analysis (else-part i) nil)))
		 (append (loop for v in todecl-now
			       collect (if (is-arg? v) (break)
					 (Cdecl v)))
			 (cons i
			       (C-down-decl-analysis (cdr l)
						     (set-difference todecl todecl-now
								     :test #'eq-C-var))))))))))


(defun C-last-analysis (f)
  (let* ((body (body f))
	 (vars (get-set (map-Ccode body #'(lambda (y) (when (C-var? y) (list y))) #'Cdecl?))))
    (setf (body f)
	  (upd-Ccode body #'(lambda (x) (unless (and (Cdecl? x)
						     (not (member (var x) vars :test #'eq-C-var)))
					  x))))
    nil))








