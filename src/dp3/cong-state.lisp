;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cong-state.lisp -- 
;; Author          : David Cyrluk, Harald Ruess
;; Created On      : 1999/09/16
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package dp3)

(defvar *indexing* nil)
(defvar *forward-chain* nil)

;; Initialization

(defun init-dp-0 (&optional strong)
  (initial-node-structures strong)
  (when strong
    (initial-term-hash))
  (init-bdd))


(deftype cong-state () T)

;; Abstract data type for the entries of congruence closure state.

(defvar *compactify-threshold* 7)

(deftype table () 'cons)

(defmacro mk-table (alist hash)
  `(cons (the list ,alist) (the hash-table ,hash)))

(defmacro table-alist (tbl) `(car (the table ,tbl)))
(defmacro table-hash (tbl) `(cdr (the table ,tbl)))

(defmacro table-alist-length (tbl)
  `(length (table-alist ,tbl)))

(defun table-init ()
   (mk-table nil (dp-make-eq-hash-table)))

(defmacro table-new (tbl)
   tbl)

(defmacro table-value (trm tbl)
  (let ((found (gensym)))
    `(let ((found (assoc (the node ,trm)
			  (the list (table-alist (the table ,tbl))))))  
       (if found (values (cdr (the cons found)) t)
	   (gethash (the node ,trm)
		    (table-hash (the table ,tbl)))))))

(defmacro update-table-value (trm tbl val index range)
  (let ((key (gensym))
	(lookup (gensym))
	(new-hash (gensym)))
    `(locally (declare (special *inline-update-flags*))
	      (declare (type simple-vector *inline-update-flags*))
	      (declare (type node ,trm))
	      (declare (type table ,tbl))
	      (declare (type fixnum ,index))
       (cond ((= (the bit (aref *inline-update-flags* ,index)) (the bit 1))
	      (setf (gethash ,trm (table-hash ,tbl)) (the ,range ,val)))
	     ((>= (the fixnum (table-alist-length (table-alist ,tbl)))
		  (the fixnum *compactify-threshold*))
	      (let ((,new-hash (copy-hash-table (table-hash ,tbl))))
		(loop for (,key . ,lookup) in (reverse (the list (table-alist ,tbl)))
		      do (setf (gethash ,key ,new-hash) ,lookup))
	       (setf (aref *inline-update-flags* ,index) (the bit 1))
	       (setq ,tbl (mk-table nil ,new-hash)))
	      ,val)
	     (t (setf (table-alist ,tbl)
		      (acons ,trm (the ,range ,val)
			     (the list (table-alist ,tbl))))
	       ,val)))))

;; Inline-update flags for tables on stack

(defvar *canon-inline-update-flag* 0)
(defvar *seen-inline-update-flag* 1)
(defvar *find-inline-update-flag* 2)
(defvar *use-inline-update-flag* 3)
(defvar *sig-inline-update-flag* 4)
(defvar *type-inline-update-flag* 5)
(defvar *extensions-inline-update-flag* 6)
(defvar *index-inline-update-flag* 7)

(defun inline-update-flags-init ()
  "Initially all inline updates are disallowed"
  (make-array 8 :element-type 'bit
	         :initial-element 0))

(defmacro inline-update-flags-new (flags)
  (let ((i (gensym))
	(new-flags (gensym)))
    `(let ((,new-flags (make-array 8 :element-type 'bit)))
       (loop for ,i from 0 below 7
	     do (setf (bit ,new-flags ,i) (bit ,flags ,i)))
       ,new-flags)))

(defvar *inline-update-flags* (inline-update-flags-init))


;; Current congurence closure state

(defvar *assertions* nil)
(defvar *canon-table* (table-init))
(defvar *seen-table* (table-init))
(defvar *find-table* (table-init))
(defvar *use-table* (table-init))
(defvar *sig-table* (table-init))
(defvar *type-table* (table-init))
(defvar *extensions-table* (table-init))
(defvar *index-table* (table-init))
(defvar *disequalities* nil)
(defvar *polyhedron* nil)
(defvar *forward-chains* nil)


;; Assertions

(defmacro assertions (cs)
  `(locally (declare (special *assertions*))
            (declare (ignore ,cs))
	   *assertions*))

(defmacro add-assertion (trm cs)
  `(locally (declare (type node ,trm)
	    (ignore ,cs))
      (push ,trm *assertions*)))
 
;; Hashing of Canonization

(defmacro canon-hash (trm cs)
  `(locally (declare (type node ,trm)
		     (ignore ,cs)
		     (special *canon-hash*))
      (table-value ,trm *canon-hash*)))
  
(defmacro setf-canon-hash (trm cs ctrm)
  `(locally (declare (type node ,trm)
		     (ignore ,cs)
		     (special *canon-hash*)
		     (type node ,ctrm))
     (update-table-value ,trm *canon-hash* ,ctrm
			 ,*canon-inline-update-flag* node)))

(defsetf canon-hash setf-canon-hash)

;; Seen

(defmacro seen (trm cs)
  `(locally (declare (type node ,trm)
		     (special *seen-table*)
		     (ignore ,cs))
     (table-value ,trm *seen-table*)))
  
(defmacro setf-seen (trm cs seen)
  (let ((cnstnt (gensym)))
    `(locally (declare (special *dp-changed*)
		       (special *index-table*)
		       (special *seen-table*)
		       (type node ,trm)
		       (type T ,seen)
		       (ignore ,cs))
       (when ,*indexing*
	 (let ((,cnstnt (head-symbol ,trm)))
	   (when (and ,cnstnt (uninterp? ,cnstnt))
	     (update-table-value ,cnstnt *index-table*
		                 (cons ,trm (table-value ,cnstnt *index-table*))
				 ,*index-inline-update-flag* list))))
       (setq *dp-changed* t)
       (update-table-value ,trm *seen-table* ,seen
			   ,*seen-inline-update-flag* T))))

(defsetf seen setf-seen)

;; Indexing.
;; An (uninterpreted) constant symbol is associated
;; with a list of terms each of which have this constant as
;; a head symbol. (indices are only set in seen.)

(defmacro index (cnstnt cs)
  `(locally (declare (type constant ,cnstnt)
		     (ignore ,cs)
		     (special *index-table*))
     (table-value ,cnstnt *index-table*)))

;; Term extension of a congruence class
;; (modified in dp-union).

(defmacro extensions (trm cs)
  `(locally (declare (type node trm)
		     (special *extensions-table*)
		     (ignore cs))
     #+dbg(assert (representative-p ,trm))
     (table-value ,trm *extensions-table*)))

;; List of disequalities

(defmacro nequals (cs)
  `(locally (declare (ignore ,cs)
		     (special *disequalities*))
      *disequalities*))

(defmacro setf-nequals (cs new-nequals)
  `(locally (declare (ignore ,cs)
		     (special *disequalities*))
      (push ,new-nequals *disequalities*)
      ,new-nequals))
   
(defsetf nequals setf-nequals)

;; Polyhedrons

(defmacro polyhedral-structure (cs)
  `(locally (declare (ignore ,cs)
		     (special *polyhedron*))
     (or *polyhedron* (initial-poly))))

(defmacro setf-polyhedral-structure (cs poly)
  `(locally (declare (ignore ,cs)
		     (type poly ,poly)
		     (special *polyhedron*))
     (setq *polyhedron* ,poly)))

(defsetf polyhedral-structure setf-polyhedral-structure)


;; Forward Chaining

(defmacro forward-chains (cs)
  `(locally (declare (ignore ,cs)
		     (special *forward-chains*))
      (or *forward-chains* (initial-forward-chains))))

(defmacro setf-forward-chains (cs fc)
  `(locally (declare (ignore ,cs)
		     (type forward-chains ,fc)
		     (special *forward-chains*))
     (setq *forward-chains* ,fc)))

(defsetf forward-chains setf-forward-chains)


;; Find

(defun dp-find (trm cs)
  (declare (type node trm)
	   (ignore cs))
  (let ((find (find-from-table trm)))
    (or find (update-table-value trm *find-table* find
				 *find-inline-update-flag* node))))

(defun find-from-table (trm)
  (declare (type node trm))
  (multiple-value-bind (new found)
      (table-value trm *find-table*)
    (if (and found (not (eq new trm)))
	(find-from-table new)
	trm)))

(defun representative-p (trm cs)
  "Tests if trm is the representative of a congruence
   class as determined by find."
  (declare (type node trm)
	   (ignore cs))
  (eq (dp-find trm cs) trm))

(defun find* (trm cs)
  (declare (type node trm)
	   (ignore cs))
  (let ((lookup (the node (table-value trm *find-table*))))
    (if lookup (find* lookup cs*) trm)))

(defun find+ (trm cs)         ; Maybe can use find* instead
  (declare (type node trm))   ; if the assertion below holds...
  (let ((lookup (the node (table-value trm *find-table*))))
    #+dbg (assert (not (eq lookup trm)))
    (if (and lookup (not (eq lookup trm)))
	(find+ lookup cs)
	trm)))

(defun setf-find* (trm cs find)
  (declare (type node trm)
	   (ignore cs)
	   (type node find))
  #+dbg(assert (not (eq trm find)))
  (update-table-value trm *find-table* find
		      *find-inline-update-flag* node))

(defsetf find* setf-find*)

;; Union

(defun dp-union (trm1 trm2 cs)
  (declare (special *dp-changed*)
	   (type node trm1)
	   (type node trm2))
  (setq *dp-changed* t)
  (let ((r1 (find+ trm1 cs))
	(r2 (find+ trm2 cs)))
    (update-table-value r2 *extensions-table*
			(append (extensions r2 cs) (extensions r1 cs))
			,*extensions-inline-update-flag* list)
    (setf (find* r1 cs) r2)))

;; Use

(defmacro use (trm cs)
  `(locally (declare (type node ,trm)
		     (ignore ,cs))
      (table-value ,trm *use-table*)))

(defmacro add-use (arg trm cs)
  `(locally (declare (type node ,trm)
		     (ignore ,cs)
		     (special *dp-changed*))
     #+dbg(assert(eq trm (sigma trm cs)))	
     (setq *dp-changed* t)
     (update-table-value ,arg *use-table*
	                 (cons ,trm (table-value ,arg *use-table*))
			 ,*use-inline-update-flag*
			 list)))
	
;; Signature

(defmacro sig (trm cs)
  `(locally (declare (type node ,trm)
		     (ignore ,cs))
    (or (table-value ,trm *sig-table*) ,trm)))

(defmacro setf-sig (trm cs sig)
  `(locally (declare (type node ,trm)
		     (ignore, cs)
		     (type node ,sig)
		     (special *dp-changed*))
     (setq *dp-changed* t)
     (update-table-value ,trm *sig-table* ,sig
			 ,*sig-inline-update-flag* node)))

(defsetf sig setf-sig)

;; Types

(defmacro dp-type (trm cs)
  `(locally (declare (type node ,trm)
		     (ignore ,cs))
     (table-value ,trm *type-table*)))

(defmacro setf-dp-type (trm cs type)
  `(locally (declare (type node ,trm)
		     (ignore ,cs)
		     (type node ,type)
		     (special *dp-changed*))
     (setq *dp-changed* t)
     (update-table-value ,trm *type-table* ,type
			 *type-inline-update-flag* node)))
 
(defsetf dp-type setf-dp-type)

(defun type-union (trm1 trm2 cs)
  (declare (type node trm1)
	   (type node trm2))
  (let ((typ1 (dp-type trm1 cs))
	(typ2 (dp-type trm2 cs)))
    (let ((merged-type (merge-type typ1 typ2)))
      (setf (dp-type trm1 cs) merged-type
	    (dp-type trm2 cs) merged-type))))
	      
(defun merge-type (typ1 typ2)
  ; Should get beefed up to deal with subtypes like integer and rational.
  (or typ1 typ2))

;; Administering a stack of congruence closure states


(defmacro with-pushed-cong-state (() &body body)
  `(let ((*inline-update-flags*  (inline-update-flags-init))
	 (*assertions* *assertions*)
	 (*canon-table* (table-new *canon-table*))
	 (*seen-table* (table-new *seen-table*))
	 (*find-table* (table-new *find-table*))
	 (*use-table* (table-new *use-table*))
	 (*sig-table* (table-new *sig-table*))
	 (*type-table* (table-new *type-table*))
         (*extensions-table* (table-new *extensions-table*))
	 (*index-table* (table-new *index-table*))
	 (*disequalities* *disequalities*)
	 (*polyhedron* (and nil (new-poly *polyhedron*)))
	 (*forward-chains* (and nil *forward-chain*
				(forward-chains-new *forward-chains*))))
     (declare (dynamic-extent *inline-update-flags*))
     (progn ,@body)))








