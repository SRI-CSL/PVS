;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arrays.lisp -- 
;; Author          : David Cyrluk
;; Created On      : 1999/09/16
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package dp)

(defun copy-hash-table (new-hash-table old-hash-table)
  (maphash #'(lambda (key val)
	       (setf (gethash key new-hash-table)
		     val))
	   old-hash-table)
  new-hash-table)

(defun copy-array (new-array old-array)
  (simple-copy-array new-array old-array))

(defun simple-copy-array (new-array old-array)
  (let* ((new-array-size (array-total-size new-array))
	 (old-array-size (array-total-size old-array))
	 (adjusted-new-array (if (< new-array-size old-array-size)
				 (make-array  old-array-size)
				 new-array)))
    (loop for i from 0 below old-array-size
	  do
	  (setf (svref adjusted-new-array i)
		(svref old-array i)))
    adjusted-new-array))

(defun general-copy-array (new-array old-array)
  (let* ((new-array-size (array-total-size new-array))
	 (old-array-size (array-total-size old-array))
	 (adjusted-new-array (if (< new-array-size old-array-size)
				 (adjust-array new-array old-array-size)
				 new-array)))
    (loop for i from 0 below old-array-size
	  do
	  (setf (row-major-aref adjusted-new-array i)
		(row-major-aref old-array i)))
    adjusted-new-array))

(defun cong-state*-entry-count (cse)
  (if *use-alists*
      (length cse)
      (hash-table-count cse)))

(defun dp-make-cong-state*-entry ()
  (if *use-alists*
      nil
      (dp-make-eq-hash-table)))

(defmacro dp-clr-cong-state*-entry (cse)
  (if *use-alists*
      `(setf ,cse nil)
      `(clrhash ,cse)))

(defmacro dp-get-from-cong-state*-entry (term cse)
  (if *use-alists*
      `(let ((found (assoc ,term ,cse :test #'eq)))
	 (if found
	     (values (cdr found) t)
	     (values nil nil)))
      `(gethash ,term ,cse)))

(defmacro dp-put-from-cong-state*-entry (term cse value)
  (if *use-alists*
      `(push (cons ,term ,value) ,cse)
      `(setf (gethash ,term ,cse)
	     ,value)))

(defdpstruct (cong-state*
	      (:print-function
	       (lambda (cs* s k)
		 (declare (ignore k))
		 (format s "<~D: ~D assertions, ~D use, ~D find, ~D sig, ~D neq, ~D type, ~D rewrites, ~D forward-chains, ~D schmas>"
		   (cong-state*-id cs*)
		   (length (cong-state*-assertions cs*))
		   (cong-state*-entry-count (cong-state*-use-hash cs*))
		   (cong-state*-entry-count (cong-state*-find-hash cs*))
		   (cong-state*-entry-count (cong-state*-sig-hash cs*))
		   (length (cong-state*-neq-list cs*))
		   (cong-state*-entry-count (cong-state*-type-hash cs*))
		   (+ (length (rewrite-rules-rules!
			       (cong-state*-rewrite-rules cs*)))
		      (length (rewrite-rules-rules
			       (cong-state*-rewrite-rules cs*))))
		   (length (forward-chains-orig-rules
			    (cong-state*-forward-chains cs*)))
		   (cong-state*-entry-count (cong-state*-schema-hash cs*)))))) ; added -h2 3/99
  (assertions nil)
  (canon-hash (dp-make-cong-state*-entry))
  (seen-hash (dp-make-cong-state*-entry))
  (find-hash (dp-make-cong-state*-entry))
  (use-hash (dp-make-cong-state*-entry))
  (sig-hash (dp-make-cong-state*-entry))
  (neq-list nil)
  (type-hash (dp-make-cong-state*-entry))
  (polyhedral-structure (initial-polyhedral-structure))
  (fourier-motzkin (initial-fourier-motzkin))
  (rewrite-rules (initial-rewrite-rules))
  (forward-chains (initial-forward-chains))
  (id 0)
  (schema-hash (dp-make-cong-state*-entry)))    ; added -hr 2/99

(defun print-cong-state* (cs* s)
  (format s "<~D assertions, ~D use, ~D find, ~D sig, ~D neq, ~D type, ~D rewrites, ~D forward-chains, ~D schemas>"
    (length (cong-state*-assertions cs*))
    (cong-state*-entry-count (cong-state*-use-hash cs*))
    (cong-state*-entry-count (cong-state*-find-hash cs*))
    (cong-state*-entry-count (cong-state*-sig-hash cs*))
    (length (cong-state*-neq-list cs*))
    (cong-state*-entry-count (cong-state*-type-hash cs*))
    (+ (length (rewrite-rules-rules! (cong-state*-rewrite-rules cs*)))
       (length (rewrite-rules-rules (cong-state*-rewrite-rules cs*))))
    (length (forward-chains-orig-rules
	     (cong-state*-forward-chains cs*)))
   (cong-state*-entry-count (cong-state*-schema-hash cs*))))

(defun clear-cong-state* (cong-state*)
  (setf (cong-state*-assertions cong-state*) nil)
  (dp-clr-cong-state*-entry (cong-state*-canon-hash cong-state*))
  (dp-clr-cong-state*-entry (cong-state*-seen-hash cong-state*))
  (dp-clr-cong-state*-entry (cong-state*-find-hash cong-state*))
  (dp-clr-cong-state*-entry (cong-state*-use-hash cong-state*))
  (dp-clr-cong-state*-entry (cong-state*-sig-hash cong-state*))
  (setf (cong-state*-neq-list cong-state*) nil)
  (dp-clr-cong-state*-entry (cong-state*-type-hash cong-state*))
  (clr-polyhedral-structure (cong-state*-polyhedral-structure cong-state*))
  (clr-fourier-motzkin (cong-state*-fourier-motzkin cong-state*))
  (clr-rewrite-rules (cong-state*-rewrite-rules cong-state*))
  (clr-forward-chains (cong-state*-forward-chains cong-state*))
  (dp-clr-cong-state*-entry (cong-state*-schema-hash cong-state*))
  cong-state*)

(defdpstruct (made-cong-states
	      (:print-function
	       (lambda (mcs s k)
		 (declare (ignore k))
		 (format s "<~D used cong states, ~D free cong states>"
		   (length (made-cong-states-used mcs))
		   (length (made-cong-states-free mcs))))))
  (used nil :type list)
  (free nil :type list))

(defun print-made-cong-states (mcs s)
  (format s "<~D used cong states, ~D free cong states>"
    (length (made-cong-states-used mcs))
    (length (made-cong-states-free mcs))))

(defun get-cong-state (made-cong-states)
  (if (made-cong-states-free made-cong-states)
      (get-free-cong-state made-cong-states)
      (get-cong-state* made-cong-states)))

(defun get-free-cong-state (made-cong-states)
  (let* ((result (pop (made-cong-states-free made-cong-states))))
    ;(clear-cong-state* result)
    (push result (made-cong-states-used made-cong-states))
    result))

(defun get-cong-state* (made-cong-states)
  (let* ((result (make-cong-state*)))
    (setf (cong-state*-id result)
	  (+ (length (made-cong-states-used made-cong-states))
	     (length (made-cong-states-free made-cong-states))))
    (push result (made-cong-states-used made-cong-states))
    result))

(defun return-cong-state* (cong-state* made-cong-states)
  (when (member cong-state* (made-cong-states-used made-cong-states)
		:test #'eq)
    (clear-cong-state* cong-state*)
    (setf (made-cong-states-used made-cong-states)
	  (delete cong-state* (made-cong-states-used made-cong-states)
		  :test #'eq))
    (push cong-state* (made-cong-states-free made-cong-states))))

(defun return-all-cong-states (made-cong-states)
  (loop for cong-state* in (made-cong-states-used made-cong-states)
	do (return-cong-state* cong-state* made-cong-states)))

(defun return-all-cong-states* (made-cong-states)
  (when (double-cons-back (made-cong-states-free made-cong-states))
    (setf (made-cong-states-free made-cong-states)
	  (double-cons-back (made-cong-states-free made-cong-states)))
    (return-all-cong-states* made-cong-states)))

(defvar *made-cong-states* (make-made-cong-states :used nil :free nil))

(defvar *print-cong-state* nil)
(defvar *print-cong-state-stack* t)

(defdpstruct (cong-state
	    (:print-function
	     (lambda (cs s k)
	       (declare (ignore k))
	       (cond
		(*print-cong-state*
		 (format s "~A"
		   (cong-state-used-assertions cs)))
		(*print-cong-state-stack*
		 (format s "<~A>"
		   (loop for cs* in (cong-state-stack cs)
			 collect (cong-state*-id cs*))))
		(t
		 (format s "<~A used assertions>"
		   (length (cong-state-used-assertions cs))))))))
  (stack nil :type list)
  (reverse nil :type list)
  (used-assertions nil :type list))

(defmethod print-cong-state (cs s)
  (if *print-cong-state*
      (format s "~A"
	(cong-state-stack cs))
      (format s "<~A used assertions>"
	(length (cong-state-used-assertions cs)))))

(defmacro top (cong-state-stack)
  `(car ,cong-state-stack))

(defmacro stack-rest (cong-state-stack)
  `(cdr ,cong-state-stack))

(defmacro previous (reverse-cong-state-stack)
  `(cdr ,reverse-cong-state-stack))


;(defvar *cong-state* (null-single-cong-state))

(defun pop-cong-state (cong-state)
  (make-cong-state :stack (cdr (cong-state-stack cong-state))
		   :reverse (reverse (cdr (cong-state-stack cong-state)))
		   ))

(defun npop-cong-state (cong-state)
  (let* ((old-reverse (cong-state-reverse cong-state))
	 (popped-cong-state* (pop (cong-state-stack cong-state)))
	 (new-reverse (nbutlast old-reverse)))
    (setf (cong-state-reverse cong-state)
	  new-reverse)
    ;;; The code below is used to make sure that when the cong-state*
    ;;; is returned and the polyhedral-structure is cleared
    ;;; we do not clear polyhedrons that are used in the rest
    ;;; of the cong-state.
    (when (domain-eq (polyhedral-structure-polyhedron
	       (cong-state*-polyhedral-structure popped-cong-state*))
	      (polyhedral-structure-polyhedron
	       (polyhedral-structure cong-state)))
      ;;; so that it is not freed when popped-cong-state* is returned.
      (setf (polyhedral-structure-polyhedron (cong-state*-polyhedral-structure
					      popped-cong-state*))
	    (universal-polyhedral-domain)))
    (when (domain-eq (polyhedral-structure-epsilon-poly
	       (cong-state*-polyhedral-structure popped-cong-state*))
	      (polyhedral-structure-epsilon-poly
	       (polyhedral-structure cong-state)))
      ;;; so that it is not freed when popped-cong-state* is returned.
      (setf (polyhedral-structure-epsilon-poly
	     (cong-state*-polyhedral-structure popped-cong-state*))
	    (initial-epsilon-poly)))
    (when (eq (polyhedral-structure-projection-matrix
	       (cong-state*-polyhedral-structure popped-cong-state*))
	      (polyhedral-structure-projection-matrix
	       (polyhedral-structure cong-state)))
      ;;; so that it is not freed when popped-cong-state* is returned.
      (setf (polyhedral-structure-projection-matrix
	     (cong-state*-polyhedral-structure popped-cong-state*))
	    (initial-projection-matrix)))
    (when (eq (polyhedral-structure-ineq-var-to-index-hash
	       (cong-state*-polyhedral-structure popped-cong-state*))
	      (polyhedral-structure-ineq-var-to-index-hash
	       (polyhedral-structure cong-state)))
      ;;; so that it is not freed when popped-cong-state* is returned.
      (setf (polyhedral-structure-ineq-var-to-index-hash
	     (cong-state*-polyhedral-structure popped-cong-state*))
	    (initial-ineq-var-to-index-hash)))
    (when (eq (polyhedral-structure-ineq-var-index-array
	       (cong-state*-polyhedral-structure popped-cong-state*))
	      (polyhedral-structure-ineq-var-index-array
	       (polyhedral-structure cong-state)))
      ;;; so that it is not freed when popped-cong-state* is returned.
      (setf (polyhedral-structure-ineq-var-index-array
	     (cong-state*-polyhedral-structure popped-cong-state*))
	    (initial-ineq-var-index-array)))
    (return-cong-state* popped-cong-state* *made-cong-states*)
    cong-state))

(defun push-new-cong-state (cong-state)
  (declare (type cong-state cong-state))
  (let ((cong-state* (get-cong-state *made-cong-states*)))
    (push-cong-state* cong-state* cong-state)))

(defmacro nprotecting-cong-state ((new-cong-state old-cong-state)
				  &body body)
  (let ((resultsym (gensym)))
    `(let ((,new-cong-state (push-new-cong-state ,old-cong-state))
	   (,resultsym nil))
       (unwind-protect
	   (setq ,resultsym
		 (multiple-value-list (progn ,@body)))
	 (npop-cong-state ,new-cong-state))
       (values-list ,resultsym))))

(defun push-cong-state* (cong-state* cong-state)
  (setf (cong-state*-neq-list cong-state*) (nequals cong-state))
  (setf (cong-state*-polyhedral-structure cong-state*)
	(copy-latest-polyhedral-structure
	 (cong-state*-polyhedral-structure cong-state*)
	 (polyhedral-structure cong-state)))
  (setf (cong-state*-fourier-motzkin cong-state*)
	(copy-fourier-motzkin
	 (cong-state*-fourier-motzkin cong-state*)
	 (fourier-motzkin cong-state)))
  (setf (cong-state*-rewrite-rules cong-state*)
	(copy-rewrite-rules-and-hash
	 (cong-state*-rewrite-rules cong-state*)
	 (rewrite-rules cong-state)))
  (setf (cong-state*-forward-chains cong-state*)
	(copy-forward-chains-and-hash
	 (cong-state*-forward-chains cong-state*)
	 (forward-chains cong-state)))
  (let* ((cong-state-stack (cong-state-stack cong-state))
	 (top-cong-state* (when cong-state-stack
			    (top cong-state-stack))))
    (when (and *use-alists* top-cong-state*)
      ;;; If using hash tables the copying is done lazily
      ;;; when find, use, etc are called.
      (setf (cong-state*-canon-hash cong-state*)
	    (cong-state*-canon-hash top-cong-state*))
      (setf (cong-state*-seen-hash cong-state*)
	    (cong-state*-seen-hash top-cong-state*))
      (setf (cong-state*-find-hash cong-state*)
	    (cong-state*-find-hash top-cong-state*))
      (setf (cong-state*-use-hash cong-state*)
	    (cong-state*-use-hash top-cong-state*))
      (setf (cong-state*-sig-hash cong-state*)
	    (cong-state*-sig-hash top-cong-state*))
      (setf (cong-state*-type-hash cong-state*)
	    (cong-state*-type-hash top-cong-state*))
      (setf (cong-state*-schema-hash cong-state*)
	    (cong-state*-schema-hash top-cong-state*))))	  
  (let* ((new-stack (cons cong-state* (cong-state-stack cong-state)))
	 (new-reverse (reverse new-stack)))
    (make-cong-state :stack new-stack :reverse new-reverse
		     :used-assertions
		     (cong-state-used-assertions cong-state))))

(defun add-assertion (assertion cong-state)
  (declare (type cong-state cong-state))
  (setf (cong-state*-assertions (top (cong-state-stack cong-state)))
	(cons assertion (cong-state*-assertions
			 (top (cong-state-stack cong-state)))))
  (setf (cong-state-used-assertions cong-state)
	(cons assertion (cong-state-used-assertions cong-state)))
  cong-state)

(defun dp-changed (old-cong-state new-cong-state)
  "Api to the outside world to determine whether two cong-states
are the same or not."
  (not (eq (cong-state-used-assertions old-cong-state)
	   (cong-state-used-assertions new-cong-state))))

#||
(defmethod print-object :around ((expr vector)) stream)
  (cond
   ((leaf-p (print-leaf expr stream)))
   ((application-p (print-application expr stream)))
   ((cong-state*-p (print-cong-state* expr stream)))
   ((double-cons-p (print-double-cons expr stream)))
   ((made-cong-states-p (print-made-cong-states expr stream)))
   ((cong-state-p (print-cong-state expr stream)))
   (t (call-next-method))))
||#


(defun setf-canon-hash* (term cong-state* term-canon)
  (declare (type node term)
	   (type cong-state* cong-state*))
  (if *use-alists*
      (push (cons term term-canon) (cong-state*-canon-hash cong-state*))
      (setf (gethash term (cong-state*-canon-hash cong-state*))
	    term-canon)))

(defun setf-canon-hash* (term cong-state* term-canon)
  (declare (type node term)
	   (type cong-state* cong-state*))
  (dp-put-from-cong-state*-entry term (cong-state*-canon-hash cong-state*)
				 term-canon))

(defsetf canon-hash* setf-canon-hash*)

(defun canon-hash (term cong-state)
  (declare (type node term)
	   (type cong-state cong-state))
  (if *use-alists*
      (canon-hash* term (top (cong-state-stack cong-state)))
      (canon-from-stack term (cong-state-stack cong-state))))

(defun canon-from-stack (term cong-state-stack)
  (declare (type node term)
	   (type list cong-state-stack))
  (if cong-state-stack
      (multiple-value-bind (canon found)
	  (canon-hash* term (top cong-state-stack))
	(if found
	    canon
	    (setf (canon-hash* term (the cong-state* (top cong-state-stack)))
		  (canon-from-stack
		   term
		   (rest cong-state-stack)))))
      nil))

(defun canon-hash* (term cong-state*)
  (declare (type node term)
	   (type cong-state* cong-state*))
  (dp-get-from-cong-state*-entry term (cong-state*-canon-hash cong-state*)))

(defun setf-canon-hash (term cong-state term-canon)
  (declare (type node term)
	   (type cong-state cong-state))
  (setf (canon-hash* term (top (cong-state-stack cong-state)))
	term-canon))

(defsetf canon-hash setf-canon-hash)

(defun setf-seen* (term cong-state* term-seen)
  (declare (type node term)
	   (type cong-state* cong-state*))
  (dp-put-from-cong-state*-entry term (cong-state*-seen-hash cong-state*)
				 term-seen))

(defsetf seen* setf-seen*)

(defun seen (term cong-state)
  (declare (type node term)
	   (type cong-state cong-state))
  (if *use-alists*
      (seen* term (top (cong-state-stack cong-state)))
      (seen-from-stack term (cong-state-stack cong-state))))

(defun seen-from-stack (term cong-state-stack)
  (declare (type node term)
	   (type list cong-state-stack))
  (if cong-state-stack
      (multiple-value-bind (seen found) (seen* term (top cong-state-stack))
	(if found
	    seen
	    (setf (seen* term (the cong-state* (top cong-state-stack)))
		  (seen-from-stack
		   term
		   (rest cong-state-stack)))))
      nil))

(defun seen* (term cong-state*)
  (declare (type node term)
	   (type cong-state* cong-state*))
  (dp-get-from-cong-state*-entry term (cong-state*-seen-hash cong-state*)))

(defun setf-seen (term cong-state term-seen)
  (declare (special *dp-changed*)
	   (type node term)
	   (type cong-state cong-state))
  (setq *dp-changed* t)
  (setf (seen* term (top (cong-state-stack cong-state)))
	term-seen))

(defsetf seen setf-seen)

(defun nequals (cong-state)
  (declare (type cong-state cong-state))
  (nequals-from-stack (cong-state-stack cong-state)))

(defun nequals-from-stack (cong-state-stack)
  (if cong-state-stack
      (nequals* (top cong-state-stack))
      nil))

(defun nequals* (cong-state*)
  (cong-state*-neq-list cong-state*))

(defun setf-nequals (cong-state new-nequals)
  (setf (cong-state*-neq-list (top (cong-state-stack cong-state)))
	new-nequals))

(defsetf nequals setf-nequals)

(defun polyhedral-structure (cong-state)
  (declare (type cong-state cong-state))
  (polyhedral-structure-from-stack (cong-state-stack cong-state)))

(defun polyhedral-structure-from-stack (cong-state-stack)
  (if cong-state-stack
      (polyhedral-structure* (top cong-state-stack))
      (initial-polyhedral-structure)))

(defun polyhedral-structure* (cong-state*)
  (cong-state*-polyhedral-structure cong-state*))

(defun setf-polyhedral-structure (cong-state new-poly)
  (setf (cong-state*-polyhedral-structure (top (cong-state-stack cong-state)))
	new-poly))

(defsetf polyhedral-structure setf-polyhedral-structure)

(defun fourier-motzkin (cong-state)
  (declare (type cong-state cong-state))
  (fourier-motzkin-from-stack (cong-state-stack cong-state)))

(defun fourier-motzkin-from-stack (cong-state-stack)
  (if cong-state-stack
      (fourier-motzkin* (top cong-state-stack))
      (initial-fourier-motzkin)))

(defun fourier-motzkin* (cong-state*)
  (cong-state*-fourier-motzkin cong-state*))

(defun setf-fourier-motzkin (cong-state new-fourier-motzkin)
  (setf (cong-state*-fourier-motzkin (top (cong-state-stack cong-state)))
	new-fourier-motzkin))

(defsetf fourier-motzkin setf-fourier-motzkin)

(defun rewrite-rules (cong-state)
  (declare (type cong-state cong-state))
  (rewrite-rules-from-stack (cong-state-stack cong-state)))

(defun rewrite-rules-from-stack (cong-state-stack)
  (if cong-state-stack
      (rewrite-rules* (top cong-state-stack))
      (initial-rewrite-rules)))

(defun rewrite-rules* (cong-state*)
  (cong-state*-rewrite-rules cong-state*))

(defun setf-rewrite-rules (cong-state new-rewrite-rules)
  (setf (cong-state*-rewrite-rules (top (cong-state-stack cong-state)))
	new-rewrite-rules))

(defsetf rewrite-rules setf-rewrite-rules)

(defun forward-chains (cong-state)
  (declare (type cong-state cong-state))
  (forward-chains-from-stack (cong-state-stack cong-state)))

(defun forward-chains-from-stack (cong-state-stack)
  (if cong-state-stack
      (forward-chains* (top cong-state-stack))
      (initial-forward-chains)))

(defun forward-chains* (cong-state*)
  (cong-state*-forward-chains cong-state*))

(defun setf-forward-chains (cong-state new-forward-chains)
  (setf (cong-state*-forward-chains (top (cong-state-stack cong-state)))
	new-forward-chains))

(defsetf forward-chains setf-forward-chains)

(defvar *reverse-find* nil) ;;; When set will do a find by first looking
                            ;;; in the oldest cong-state*, and
                            ;;; then propogating back up.
                            ;;; Otherwise, it first looks in the top
                            ;;; cong-state* and if it finds the find
                            ;;; there it stops.
                            ;;; If it doesn't it recursively looks for
                            ;;; the find in the rest of the stack and
                            ;;; memoizes the result as it unwinds the stack.

(defun setf-find* (term cong-state* term-find)
  #+dbg (assert (not (eq term term-find)))
  (dp-put-from-cong-state*-entry term (cong-state*-find-hash cong-state*)
				 term-find))

(defsetf find* setf-find*)

(defun dp-find (term cong-state)
  (if *reverse-find*
      (find-from-reverse-stack term
			       (cong-state-reverse cong-state))
      (find-from-stack-top term (cong-state-stack cong-state))))

(defun find-from-stack-top (term cong-state-stack)
  (let ((find (if *use-alists*
		  (find-from-alists
		   term (cong-state*-find-hash (top cong-state-stack)))
		  (find-from-stack term cong-state-stack cong-state-stack))))
    (or find
	(setf (find* term cong-state-stack)
	      find))))

(defun find-from-alists (term findalist)
  (multiple-value-bind (new-term found)
      (dp-get-from-cong-state*-entry term findalist)
    (if (and found (not (eq new-term term)))
	(find-from-alists new-term findalist)
	term)))

(defun find-from-stack (term cong-state-stack top-cong-state-stack)
  ;;; See comments under *reverse-find* defvar.
  (if cong-state-stack
      (let ((find (find-for* term (top cong-state-stack))))
	(if find
	    (if (or (eq cong-state-stack top-cong-state-stack)
		    (eq find term))
		find
		;;; In this case we found a find for term different
		;;; than term in a lower part of the stack.
		;;; We must now check to see if this find has a find
		;;; in the upper part of the stack.
		(find-from-stack-top find top-cong-state-stack))
	    (let ((rest-find (find-from-stack term (rest cong-state-stack)
					      cong-state-stack)))
	      (cond
	       (rest-find
		(setf (find* term (top cong-state-stack))
		      rest-find)
		(find-from-stack-top rest-find top-cong-state-stack))
	       (t (setf (find* term (top cong-state-stack))
			term))))))
      nil))

(defun find-from-reverse-stack (term cong-state-stack)
  ;;; See comments under *reverse-find* defvar.
  (if cong-state-stack
      (let ((new-term (find+ term (top cong-state-stack))))
	(find-from-reverse-stack new-term (previous cong-state-stack)))
      term))

(defun find-for* (term cong-state*)
  (let ((hash-term (gethash term (cong-state*-find-hash cong-state*))))
    (if hash-term
	(if (not (eq hash-term term))
	    (find+ hash-term cong-state*)
	    hash-term)
	nil)))

(defun find* (term cong-state*)
  (let ((hash-term (dp-get-from-cong-state*-entry
		    term (cong-state*-find-hash cong-state*))))
    (if hash-term
	(find* hash-term cong-state*)
	term)))

(defun find+ (term cong-state*)
  ;;; Maybe can use find* instead.
  ;;; If the assert below holds.
  (let ((hash-term (dp-get-from-cong-state*-entry
		    term (cong-state*-find-hash cong-state*))))
    #+dbg (assert (not (eq hash-term term)))
    (if (and hash-term (not (eq hash-term term)))
	(find+ hash-term cong-state*)
	term)))

;;; Harald says schemas can go.
(defun setf-schema* (term cong-state* term-schema)
  (dp-put-from-cong-state*-entry term (cong-state*-schema-hash cong-state*)
				 term-schema))

(defsetf schema* setf-schema*)

(defun dp-schema (term cong-state)
  (if *reverse-find*
      (schema-from-reverse-stack term
                               (cong-state-reverse cong-state))
      (schema-from-stack-top term (cong-state-stack cong-state))))

(defun schema-from-stack-top (term cong-state-stack)
  (let ((schema (if *use-alists*
		    (schema-from-alists
		     term (cong-state*-schema-hash (top cong-state-stack)))
		    (schema-from-stack
		     term cong-state-stack cong-state-stack))))
    (or schema
        (setf (schema* term cong-state-stack)
              schema))))

(defun schema-from-alists (term schema-alist)
  (multiple-value-bind (new-term found)
      (dp-get-from-cong-state*-entry term schema-alist)
    (if found
	(schema-from-alists new-term schema-alist)
	term)))

(defun schema-from-stack (term cong-state-stack top-cong-state-stack)
  (if cong-state-stack
      (let ((schema (schema-for* term (top cong-state-stack))))
        (if schema
            (if (or (eq cong-state-stack top-cong-state-stack)
                    (eq schema term))
                schema
                (schema-from-stack-top schema top-cong-state-stack))
            (let ((rest-schema (schema-from-stack term (rest cong-state-stack)
                                              cong-state-stack)))
              (cond
               (rest-schema
                (setf (schema* term (top cong-state-stack))
                      rest-schema)
                (schema-from-stack-top rest-schema top-cong-state-stack))
               (t (setf (schema* term (top cong-state-stack))
                        term))))))
      nil))

(defun schema-from-reverse-stack (term cong-state-stack)
  (if cong-state-stack
      (let ((new-term (schema+ term (top cong-state-stack))))
        (schema-from-reverse-stack new-term (previous cong-state-stack)))
      term))

(defun schema-for* (term cong-state*)
  (let ((hash-term (gethash term (cong-state*-schema-hash cong-state*))))
    (if hash-term
        (if (not (eq hash-term term))
            (schema+ hash-term cong-state*)
            hash-term)
        nil)))

(defun schema* (term cong-state*)
  (let ((hash-term (dp-get-get-from-cong-state*-entry
		    term (cong-state*-schema-hash cong-state*))))
    (if hash-term
        (schema* hash-term cong-state*)
        term)))

(defun schema+ (term cong-state*)
  (let ((hash-term (dp-get-from-cong-state*-entry
		    term (cong-state*-schema-hash cong-state*))))
    (if (and hash-term (not (eq hash-term term)))
        (schema+ hash-term cong-state*)
        term)))

(defun dp-union (term1 term2 cong-state)
  (declare (special *dp-changed*))
  (setq *dp-changed* t)
  (union* term1 term2 (top (cong-state-stack cong-state))))

(defun union* (term1 term2 cong-state*)
  (when (dp-variable-p (schema+ term2 cong-state*)) ; added -hr 2/99
    (setf (schema* (schema+ (find+ term2 cong-state*) cong-state*) cong-state*)
	  (schema+ (find+ term1 cong-state*) cong-state*)))
  (setf (find* (find+ term1 cong-state*) cong-state*)
	(find+ term2 cong-state*)))


(defun setf-use* (term cong-state* term-use)
  (assert term-use)
  (dp-put-from-cong-state*-entry term (cong-state*-use-hash cong-state*)
				 term-use))

(defsetf use* setf-use*)

(defun use (term cong-state)
  (if *use-alists*
      (use* term (top (cong-state-stack cong-state)))
      (use-from-stack term (cong-state-stack cong-state))))

(defun use-from-stack (term cong-state-stack)
  (if cong-state-stack
      (let ((use (use* term (top cong-state-stack))))
	(or use
	    (let ((rest-use (use-from-stack term (rest cong-state-stack))))
	      (when rest-use
		(setf (use* term (top cong-state-stack))
		      rest-use)))))
      nil))

(defun use* (term cong-state*)
  (let ((hash-use (dp-get-from-cong-state*-entry
		   term (cong-state*-use-hash cong-state*))))
    hash-use))

(defun setf-use (term cong-state term-use)
  (declare (special *dp-changed*))
  (setq *dp-changed* t)
  (setf (use* term (top (cong-state-stack cong-state)))
	term-use))

(defsetf use setf-use)

(defun add-top-use (arg term cong-state)
  (declare (special *dp-changed*))
  (setq *dp-changed* t)
  (use arg cong-state) ; This is to ensure that the uses have been
                       ; copied to all the stacked hash-tables.
  (add-top-use-to-stack arg term (cong-state-stack cong-state)))

(defun add-top-use-to-stack (arg term cong-state-stack)
  (when cong-state-stack
    (add-use* arg term (top cong-state-stack))
    (add-top-use-to-stack arg term (rest cong-state-stack))))

(defun add-use* (arg term cong-state*)
  (assert (not (equality-p term)))
  (setf (use* arg cong-state*)
	(adjoin term (use* arg cong-state*) :test #'eq)))

(defun add-use (arg term cong-state)
  ;(assert (eq term (sigma term cong-state)))
  (declare (special *dp-changed*))
  (setq *dp-changed* t)
  (setf (use* arg (top (cong-state-stack cong-state)))
	(adjoin term (use arg cong-state) :test #'eq)))

(defun setf-sig* (term cong-state* term-sig)
  (dp-put-from-cong-state*-entry term (cong-state*-sig-hash cong-state*)
				term-sig))

(defsetf sig* setf-sig*)

(defun sig (term cong-state)
  (if *use-alists*
      (or (sig* term (top (cong-state-stack cong-state)))
	  term)
      (sig-from-stack term (cong-state-stack cong-state))))

(defun sig-from-stack (term cong-state-stack)
  (if cong-state-stack
      (let ((sig (sig* term (top cong-state-stack))))
	(or sig (setf (sig* term (top cong-state-stack))
		      (sig-from-stack term (rest cong-state-stack)))))
      term))

(defun sig* (term cong-state*)
  (let ((hash-sig (dp-get-from-cong-state*-entry
		   term (cong-state*-sig-hash cong-state*))))
    hash-sig))

(defun setf-sig (term cong-state term-sig)
  (declare (special *dp-changed*))
  (setq *dp-changed* t)
  (setf (sig* term (top (cong-state-stack cong-state)))
	term-sig))

(defsetf sig setf-sig)

(defun setf-type* (term cong-state* term-type) ;(break)
  (dp-put-from-cong-state*-entry term (cong-state*-type-hash cong-state*)
			      term-type))

(defsetf type* setf-type*)

(defun dp-type (term cong-state)
  (if *use-alists*
      (type* term (top (cong-state-stack cong-state)))
      (type-from-stack term (cong-state-stack cong-state))))

(defun type-from-stack (term cong-state-stack)
  (if cong-state-stack
      (let ((type (type* term (top cong-state-stack))))
	(or type (setf (type* term (top cong-state-stack))
		       (type-from-stack term (rest cong-state-stack)))))
      (node-initial-type term)))

(defun type* (term cong-state*)
  (let ((hash-type (dp-get-from-cong-state*-entry
		    term (cong-state*-type-hash cong-state*))))
    hash-type))

(defun setf-dp-type (term cong-state term-type)
  (declare (special *dp-changed*))
  (setq *dp-changed* t)
  (setf (type* term (top (cong-state-stack cong-state)))
	term-type))

(defsetf dp-type setf-dp-type)

(defun type-union (term1 term2 cong-state)
  (let ((type1 (dp-type term1 cong-state))
	(type2 (dp-type term2 cong-state)))
    (let ((merged-type (merge-type type1 type2)))
      ;(when (eq term1 *t1*) (break))
      (setf (dp-type term1 cong-state) merged-type
	    (dp-type term2 cong-state) merged-type))))
	      
(defun merge-type (type1 type2)
  ;;; Should get beefed up to deal with subtypes like integer and rational.
  (or type1 type2))

(defun findalist* (cong-state*)
  (loop for term being the hash-key in (cong-state*-find-hash cong-state*)
	using (hash-value find)
	unless (eq term find)
	collect (cons term find))))

(defun findalist (cong-state)
  "Useful for debugging to extract a simple alist from the cong-state."
  (if *use-alists*
      (and (cong-state-stack cong-state)
	   (cong-state*-find-hash (top (cong-state-stack cong-state))))
      (findalist-from-stack (cong-state-stack cong-state))))

(defun findalist-from-stack (cong-state-stack)
  (if cong-state-stack
      (nconc (findalist* (top cong-state-stack))
	     (findalist-from-stack (rest cong-state-stack)))
      nil))

(defun usealist* (cong-state*)
  (loop for term being the hash-key in (cong-state*-use-hash cong-state*)
	using (hash-value use)
	collect (cons term use)))

(defun usealist (cong-state)
  "Useful for debugging to extract a simple alist from the cong-state."
  (if *use-alists*
      (and (cong-state-stack cong-state)
	   (cong-state*-use-hash (top (cong-state-stack cong-state))))
      (usealist-from-stack (cong-state-stack cong-state))))

(defun usealist-from-stack (cong-state-stack)
  (if cong-state-stack
      (nconc (usealist* (top cong-state-stack))
	     (usealist-from-stack (rest cong-state-stack)))
      nil))

(defun sigalist* (cong-state*)
  (loop for term being the hash-key in (cong-state*-sig-hash cong-state*)
	using (hash-value sig)
	collect (cons term sig)))

(defun sigalist (cong-state)
  "Useful for debugging to extract a simple alist from the cong-state."
  (if *use-alists*
      (and (cong-state-stack cong-state)
	   (cong-state*-sig-hash (top (cong-state-stack cong-state))))
      (sigalist-from-stack (cong-state-stack cong-state))))

(defun sigalist-from-stack (cong-state-stack)
  (if cong-state-stack
      (nconc (sigalist* (top cong-state-stack))
	     (sigalist-from-stack (rest cong-state-stack)))
      nil))

(defun typealist* (cong-state*)
  (loop for term being the hash-key in (cong-state*-type-hash cong-state*)
	using (hash-value type)
	collect (cons term type)))

(defun typealist (cong-state)
  "Useful for debugging to extract a simple alist from the cong-state."
  (if *use-alists*
      (and (cong-state-stack cong-state)
	   (cong-state*-type-hash (top (cong-state-stack cong-state))))
      (typealist-from-stack (cong-state-stack cong-state))))

(defun typealist-from-stack (cong-state-stack)
  (if cong-state-stack
      (nconc (typealist* (top cong-state-stack))
	     (typealist-from-stack (rest cong-state-stack)))
      nil))

(defun null-cong-state ()
  (make-cong-state :stack nil
		   :reverse nil
		   :used-assertions nil))

(defun single-cong-state (cong-state*)
  (let ((stack (list cong-state*)))
    (setf (cong-state*-polyhedral-structure cong-state*)
	  (initial-polyhedral-structure))
    (make-cong-state :stack stack
		     :reverse stack)))

(defun null-single-cong-state ()
  (single-cong-state (get-cong-state *made-cong-states*)))

(defun init-dp-0 (&optional strong)
  (when strong
    (clrhash *term-hash*)
  ;(setq *max-node-index* 0)
    (setq *made-cong-states* (make-made-cong-states :used nil :free nil)))
  ;(return-all-cong-states *made-cong-states*)
  (setq *ineq-var-count* 0)
  (setq *epsilon* (make-epsilon))
  (setq *universal-polyhedral-domain*
	(make-universal-polyhedron *max-ineq-vars* *max-rays*))
  (setq *epsilon-leq-0-poly*
	(make-epsilon-leq-0-polyhedron *max-ineq-vars* *max-rays*))
  (setq *=* (mk-predicate-sym '=))
  (setq *true* (mk-constant 'TRUE))
  (setq *false* (mk-constant 'FALSE))
  (setq *plus* (mk-arith-operator 'plus))
  (setq *times* (mk-arith-operator 'times))
  (setq *divide* (mk-arith-operator 'divide))
  (setq *minus* (mk-arith-operator 'minus))
  (setq *difference* (mk-arith-operator 'difference))
  (setq *sup* (mk-arith-operator 'sup))
  (setq *inf* (mk-arith-operator 'inf))
  (setq *floor* (mk-arith-operator 'floor))
  (setq *ceiling* (mk-arith-operator 'ceiling))
  (setq *arithfuns*
	(list *plus* *times* *divide* *minus* *difference* *sup* *inf*))
  (setq *lesseqp* (mk-arith-pred 'lesseqp))
  (setq *lessp* (mk-arith-pred 'lessp))
  (setq *greatereqp* (mk-arith-pred 'greatereqp))
  (setq *greaterp* (mk-arith-pred 'greaterp))
  (setq *arithpreds*
	(list *lesseqp* *lessp* *greaterp* *greatereqp*))
  (setq *not* (mk-predicate-sym 'not))
  (setq *nequal* (mk-predicate-sym 'nequal))
  (setq *if* (mk-interpreted-constant 'if))
  (setq *and* (mk-predicate-sym 'and))
  (setq *or* (mk-predicate-sym 'or))
  (setq *preds*
	(list *not* *and* *or* *nequal*
	      *lesseqp* *lessp* *greaterp* *greatereqp*))
  (setq *zero* (mk-dp-number 0))
  (setq *one* (mk-dp-number 1))
  (setq *neg-one* (mk-dp-number -1))
  (setq *update* (mk-array-operator 'update))
  (setq *tuple* (mk-interpreted-constant 'tuple 'tuple-op))
  (setq *record* (mk-interpreted-constant 'record 'record-op))
  (setq *project* (mk-interpreted-constant 'project 'project-op))
  (setq *th-app* (mk-interpreted-constant 'th-app 'th-app-op))
  (setq *lambda* (mk-constant 'lambda)))
