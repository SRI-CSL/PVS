;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arrays.lisp -- 
;; Author          : David Cyrluk
;; Created On      : 1998/12/01 19:14:57
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package dp)


(defstruct (automaton-entry (:type vector))
  (slot 0 :type integer)
  (arg 0 :type integer)
  (symbol nil :type leaf)
  (argument-slot -1 :type integer))

(defvar *sae-hash* (dp-make-eq-hash-table))

(defun index-operator (term)
  (cond
   ((constant-p term)
    term)
   ((th-app-p term)
    (index-operator (arg 1 term)))
   (t (index-operator (funsym term)))))

(defun add-to-rewrite-index (rewrite rewrite-rules)
  (let ((index-op (index-operator (rr-lhs rewrite)))
	(index-hash (rewrite-rules-index-hash rewrite-rules)))
    (setf (gethash index-op index-hash)
	  (pushnew rewrite (gethash index-op index-hash)))))

(defun optimize-rewrite (rewrite rewrite-rules)
  (make-rewrite-automata rewrite)
  (add-to-rewrite-index rewrite rewrite-rules))

(defun add-rewrite-rules (rewrites rewrites! cong-state)
  (loop for r in rewrites
	do (optimize-rewrite r (rewrite-rules cong-state)))
  (loop for r in rewrites!
	do (optimize-rewrite r (rewrite-rules cong-state)))
  (setf (rewrite-rules-rules! (rewrite-rules cong-state)) rewrites!
	(rewrite-rules-rules (rewrite-rules cong-state)) rewrites))

(defun make-rewrite-automata (rewrite)
  (declare (ignore matching-automaton cond-subst-automaton))
  (clrhash *sae-hash*)
  (multiple-value-bind (matching-automaton max-vars)
      (make-matching-rewrite-automaton rewrite)
    (multiple-value-bind (cond-subst-automaton cond-max-subs)
	(make-substitution-condition-automaton rewrite max-vars)
      (make-substitution-rhs-automaton rewrite cond-max-subs))))

(defun make-matching-rewrite-automaton (rewrite)
  (multiple-value-bind (automaton max-slots max-var)
      (make-matching-automaton (rr-lhs rewrite))
    (setf (rr-matching-automaton rewrite) automaton)
    (setf (rr-matching-slots rewrite) (make-array (1+ max-slots)))
    (values automaton max-var)))

(defun make-matching-automaton (term)
  (let* ((max-slot 0)
	 (max-var -1)
	 (constant-entries (list nil))
	 (last-of-constant-entries constant-entries)
	 (variable-entries (list nil))
	 (last-of-variable-entries variable-entries))
    (declare (special max-slot max-var
		      last-of-constant-entries
		      last-of-variable-entries))
    (node-to-matching-automaton term 0 0)
    (setf (cdr last-of-constant-entries) (cdr variable-entries))
    (values (cdr constant-entries)
	    max-slot max-var)))

(defun add-to-end-of-list* (item list-end)
  (setf (cdr list-end) (list item))
  (last (cdr list-end)))

(defmacro add-to-end-of-list (item list-end)
  `(setq ,list-end
	 (add-to-end-of-list* ,item ,list-end)))

(defun node-to-matching-automaton (term slot arg)
  (declare (special last-of-constant-entries last-of-variable-entries))
  (cond
   ((application-p term)
    (make-matching-automaton* term slot arg))
   ((constant-p term)
    (add-to-end-of-list (constant-to-automaton-entry term slot arg)
			last-of-constant-entries))
   ((dp-variable-p term)
    (add-to-end-of-list (variable-to-automaton-entry term slot arg)
			last-of-variable-entries))
   (t (break))))
     
(defun make-matching-automaton* (term slot arg)
  (declare (special max-slot))
  (if (application-p (funsym term))
      (ho-make-matching-automaton* term slot arg)
      (cons (application-to-automaton-entry term slot arg (incf max-slot))
	    (args-to-matching-automaton
	     (application-arguments term) max-slot))))

(defun application-to-automaton-entry (term slot arg argument-slot)
  (declare (special last-of-constant-entries))
  (add-to-end-of-list
   (make-automaton-entry :slot slot :arg arg :symbol (funsym term)
			 :argument-slot argument-slot)
   last-of-constant-entries))

(defun args-to-matching-automaton (args slot)
  (loop for arg from 1 below (length args)
	do (node-to-matching-automaton (nth arg args) slot arg)))

(defun ho-make-matching-automaton* (term slot arg)
  (declare (special max-slot))
  (cons (ho-application-to-automaton-entry term slot arg (incf max-slot))
	(ho-args-to-matching-automaton
	 (application-arguments term) max-slot)))

(defun ho-application-to-automaton-entry (term slot arg argument-slot)
  (declare (special last-of-constant-entries)
	   (ignore term))
  (add-to-end-of-list
   (make-automaton-entry :slot slot :arg arg :symbol nil
			 :argument-slot argument-slot)
   last-of-constant-entries))

(defun ho-args-to-matching-automaton (args slot)
  (loop for arg from 0 below (length args)
	do (node-to-matching-automaton (nth arg args) slot arg)))

(defun constant-to-automaton-entry (constant slot arg)
  (make-automaton-entry :slot slot :arg arg :symbol constant))

(defun variable-to-automaton-entry (variable slot arg)
  (declare (special max-var))
  (let ((var-num (gethash variable *sae-hash*)))
    (if var-num
	(make-automaton-entry :slot slot :arg arg :symbol variable
			      :argument-slot var-num)
	(progn
	  (setf (gethash variable *sae-hash*)
		(incf max-var))
	  (make-automaton-entry :slot slot :arg arg :symbol variable
				:argument-slot max-var)))))

(defun make-substitution-array (rewrite)
  (make-array (1+ (rr-rhs-loc rewrite))))

(defun match-using-automaton (term rewrite)
  (let ((automaton (rr-matching-automaton rewrite))
	(slots (rr-matching-slots rewrite))
	(subst (make-substitution-array rewrite))
	(success t))
    (declare (special subst))
    (setf (aref slots 0) (if (or t (leaf-p term))
			     (make-array 1 :initial-element term)
			     (application-arguments term)))
    (setq success
	  (catch 'fail
	    (loop for entry in automaton do
		  (execute-automaton-entry entry slots)
		  finally (return t))))
    (if success 
	(values subst t)
	(values subst nil))))


(defun automaton-entry-type (entry)
  (let ((entry-symbol (automaton-entry-symbol entry)))
    (cond
     ((null entry-symbol) 'ho)
     ((constant-p entry-symbol)
      (if (>= (automaton-entry-argument-slot entry) 0)
	  'application
	  'constant))
     ((dp-variable-p entry-symbol) 'variable)
     (t (break)))))

(defun execute-automaton-entry (entry slots)
  (let ((entry-type (automaton-entry-type entry)))
    (case entry-type
      (ho (execute-ho-automaton-entry entry slots))
      (application (execute-application-automaton-entry entry slots))
      (constant (execute-constant-automaton-entry entry slots))
      (variable (execute-variable-automaton-entry entry slots)))))

(defun execute-application-automaton-entry (entry slots)
  (let ((term (aref (aref slots (automaton-entry-slot entry))
		    (automaton-entry-arg entry))))
    (if (application-p term)
	(let ((term-funsym (funsym term))
	      (term-arguments (application-arguments term))
	      (pattern-funsym (automaton-entry-symbol entry)))
	  (if (eq term-funsym pattern-funsym)
	      (setf (aref slots (automaton-entry-argument-slot entry))
		    term-arguments)
	      (throw 'fail nil)))
	(throw 'fail nil))))

(defun execute-ho-automaton-entry (entry slots)
  (let ((term (aref (aref slots (automaton-entry-slot entry))
		    (automaton-entry-arg entry))))
    (if (application-p term)
	(let ((term-funsym (funsym term))
	      (term-arguments (application-arguments term)))
	  (if (application-p term-funsym)
	      (setf (aref slots (automaton-entry-argument-slot entry))
		    term-arguments)
	      (throw 'fail nil)))
	(throw 'fail nil))))

(defun execute-constant-automaton-entry (entry slots)
  (let ((term (aref (aref slots (automaton-entry-slot entry))
		    (automaton-entry-arg entry))))
    (if (constant-p term)
	(unless (eq term (automaton-entry-symbol entry))
	  (throw 'fail nil))
	(throw 'fail nil))))

(defun execute-variable-automaton-entry (entry slots)
  (declare (special subst))
  (let* ((term (aref (aref slots (automaton-entry-slot entry))
		     (automaton-entry-arg entry)))
	 (variable-subst-pos (automaton-entry-argument-slot entry))
	 (substituted-variable (aref subst variable-subst-pos)))
    (if substituted-variable
	(unless (eq term substituted-variable)
	  (throw 'fail nil))
	(setf (aref subst variable-subst-pos)
	      term))))

(defstruct (substitution-automaton-entry (:conc-name "SAE-") (:type vector))
  (term nil :type leaf)
  (arguments nil :type list)
  (number 0 :type integer))

(defun make-substitution-automaton (term max-var)
  (let* ((max-entries max-var)
	 (substitution-automaton (list nil))
	 (substitution-automaton-last substitution-automaton))
    (declare (special max-entries substitution-automaton-last))
    (node-to-sa term)
    (values (cdr substitution-automaton) max-entries)))

(defun node-to-sa (term)
  (cond
   ((application-p term)
    (application-to-sa term))
   ((constant-p term)
    (constant-to-sa term))
   ((dp-variable-p term)
    nil)
   (t (break))))

(defun constant-to-sa (term)
  (declare (special substitution-automaton-last))
  (unless (gethash term *sae-hash*)
    (add-to-end-of-list (constant-to-sae term)
			substitution-automaton-last)))

(defun constant-to-sae (term)
  (declare (special max-entries))
  (let ((result (make-substitution-automaton-entry
		 :term term
		 :arguments nil
		 :number (incf max-entries))))
    (setf (gethash term *sae-hash*) max-entries)
    result))

(defun application-to-sa (term)
  (declare (special max-entries substitution-automaton-last))
  (unless (gethash term *sae-hash*)
    (map-args #'node-to-sa term)
    (let* ((arguments (map-args-list
		       #'(lambda (a) (gethash a *sae-hash*))
		       term))
	   (entry (make-substitution-automaton-entry
		   :term term
		   :arguments arguments
		   :number (incf max-entries))))
      (setf (gethash term *sae-hash*) max-entries)
      (add-to-end-of-list entry substitution-automaton-last))))

(defun make-substitution-condition-automaton (rewrite first-sub)
  (if (rr-condition rewrite)
      (multiple-value-bind (cond-subst-automaton cond-max-subs)
	  (make-substitution-automaton (rr-condition rewrite) first-sub)
	(setf (rr-condition-automaton rewrite)
	      cond-subst-automaton)
	(setf (rr-condition-loc rewrite) cond-max-subs)
	(values cond-subst-automaton cond-max-subs))
      (values nil first-sub)))

(defun make-substitution-rhs-automaton (rewrite first-sub)
  (if (if-p (rr-rhs rewrite))
      (make-substitution-rhs-if-automaton rewrite first-sub)
      (make-substitution-rhs-non-if-automaton rewrite first-sub)))

(defun make-substitution-rhs-non-if-automaton (rewrite first-sub)
  (multiple-value-bind (automaton max-subs)
      (make-substitution-automaton (rr-rhs rewrite) first-sub)
    (setf (rr-rhs-automaton rewrite) automaton)
    (setf (rr-rhs-loc rewrite) max-subs)
    (values automaton max-subs)))

(defun get-subst-loc (term)
  (gethash term *sae-hash*))

(defun make-substitution-rhs-if-automaton (rewrite first-sub)
  (let* ((rhs (rr-rhs rewrite))
	 (if-cond (if-cond rhs))
	 (if-then (if-then rhs))
	 (if-else (if-else rhs)))
    (multiple-value-bind (if-const-automaton first-sub)
	(make-substitution-automaton (funsym rhs) first-sub)
      (multiple-value-bind (cond-automaton cond-loc)
	  (make-substitution-automaton if-cond first-sub)
	(multiple-value-bind (then-automaton then-loc)
	    (make-substitution-automaton if-then cond-loc)
	  (multiple-value-bind (else-automaton else-loc)
	      (make-substitution-automaton if-else then-loc)
	    (multiple-value-bind (rhs-automaton max-subs)
		(make-substitution-automaton rhs else-loc)
	      (setf (rr-rhs-automaton rewrite)
		    (append if-const-automaton
			    cond-automaton
			    then-automaton
			    else-automaton
			    rhs-automaton))
	      (setf (rr-rhs-loc rewrite) (get-subst-loc rhs))
	      (setf (rr-if-cond-automaton rewrite)
		    (append if-const-automaton
			    cond-automaton))
	      (setf (rr-if-cond-loc rewrite) (get-subst-loc if-cond))
	      (setf (rr-if-then-automaton rewrite)
		    (append if-const-automaton
			    cond-automaton
			    then-automaton))
	      (setf (rr-if-then-loc rewrite) (get-subst-loc if-then))
	      (setf (rr-if-else-automaton rewrite)
		    (append if-const-automaton
			    cond-automaton
			    then-automaton
			    else-automaton))
	      (setf (rr-if-else-loc rewrite) (get-subst-loc if-else))
	      (values (rr-rhs-automaton rewrite) max-subs))))))))

(defun substitute-rhs-using-automaton (rewrite subst)
  (let ((automaton (rr-rhs-automaton rewrite))
	(loc (rr-rhs-loc rewrite)))
    (substitute-using-automaton automaton loc subst)))

(defun substitute-condition-using-automaton (rewrite subst)
  (let ((automaton (rr-condition-automaton rewrite))
	(loc (rr-condition-loc rewrite)))
    (substitute-using-automaton automaton loc subst)))

(defun substitute-if-cond-using-automaton (rewrite subst)
  (let ((automaton (rr-if-cond-automaton rewrite))
	(loc (rr-if-cond-loc rewrite)))
    (substitute-using-automaton automaton loc subst)))

(defun substitute-if-then-using-automaton (rewrite subst)
  (let ((automaton (rr-if-then-automaton rewrite))
	(loc (rr-if-then-loc rewrite)))
    (substitute-using-automaton automaton loc subst)))

(defun substitute-if-else-using-automaton (rewrite subst)
  (let ((automaton (rr-if-else-automaton rewrite))
	(loc (rr-if-else-loc rewrite)))
    (substitute-using-automaton automaton loc subst)))

(defun substitute-using-automaton (automaton loc subst)
  (loop for entry in automaton
	do (execute-subst-entry entry subst))
  (aref subst loc))

(defun execute-constant-subst-entry (new-pos const subst)
  (setf (aref subst new-pos) const))

(defun execute-application-subst-entry (new-pos arg-posns subst)
  (labels
      ((get-arg-from-pos
	(pos)
	(aref subst pos)))
    (let* ((arguments (mapcar #'get-arg-from-pos arg-posns))
	   (arg-array (mk-arg-array arguments))
	   (new-term (mk-term* arg-array nil)))
      (setf (aref subst new-pos) new-term))))

(defun execute-subst-entry (entry subst)
  (let ((new-pos (sae-number entry))
	(arg-posns (sae-arguments entry)))
    (if arg-posns
	(execute-application-subst-entry new-pos arg-posns subst)
	(execute-constant-subst-entry new-pos (sae-term entry) subst))))

