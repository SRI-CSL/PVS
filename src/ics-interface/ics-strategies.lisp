(in-package :pvs)

(defun ics-current-state (ps)
  (if (eql *current-decision-procedure* 'ics)
      (dp-state ps)
    (ics-empty-state)))

;; Replace formula with a simplified version

; (addrule 'sigmatize nil ((fnums *))
;   (sigmatize-step fnums)
;   "Replace formula with a simplified formula")

(defun sigmatize-step (fnums)
  #'(lambda (ps)
      (let* ((sforms (s-forms (current-goal ps)))
	     (selected-sforms (select-seq sforms fnums))
	     (remaining-sforms (delete-seq sforms fnums)))
	(multiple-value-bind (signal newform)
	    (sigmatize-sforms selected-sforms)
	  (case signal
	    (! (values '! nil))
	    (X (values 'X (current-goal ps)))
	    (? (values '? (list
			   (lcopy (current-goal ps)
			     's-forms (cons newform remaining-sforms))))))))))

(defun sigmatize-sforms (sforms)
  (let* ((fmla (make!-disjunction* (mapcar #'formula sforms)))
	 (newfmla (ics-sigma fmla)))
    (if (tc-eq fmla newfmla)
	(values 'X nil)
	(values '? (make-instance 's-formula 'formula newfmla)))))


;; Replace formula with canonized version

;(addrule 'canon nil ((fnums *))
;   (canon-step fnums)
;   "Replace formula with a simplified formula")

(defun canon-step (fnums)
  #'(lambda (ps)
      (let* ((state (ics-current-state ps))
	     (sforms (s-forms (current-goal ps)))
	     (selected-sforms (select-seq sforms fnums))
	     (remaining-sforms (delete-seq sforms fnums)))
	(multiple-value-bind (signal newform)
	    (canon-sforms state selected-sforms)
	  (case signal
	    (! (values '! nil))
	    (X (values 'X (current-goal ps)))
	    (? (values '? (list
			   (lcopy (current-goal ps)
			     's-forms (cons newform remaining-sforms))))))))))

(defun canon-sforms (state sforms)
  (let* ((fmla (make!-disjunction* (mapcar #'formula sforms)))
	 (newfmla (ics-canon state fmla)))
    (if (tc-eq fmla newfmla)
	(values 'X nil)
	(values '? (make-instance 's-formula 'formula newfmla)))))


;; Checking if a subset of the formulas hold or not

;(addrule 'check nil ((fnums *) (vars nil))
;   (check-step fnums vars)
;   "Check if argument formulas hold")

(defun check-step (fnums vars)
  #'(lambda (ps)
      (let* ((state (ics-current-state ps))
	     (sforms (s-forms (current-goal ps)))
	     (selected-sforms (select-seq sforms fnums))
	     (remaining-sforms (delete-seq sforms fnums)))
	(multiple-value-bind (signal newform)
	    (check-sforms state selected-sforms vars)
	  (case signal
	    (! (values '! nil))
	    (X (values 'X (current-goal ps)))
	    (? (values '? (list
			   (lcopy (current-goal ps)
			     's-forms (cons newform remaining-sforms))))))))))

(defun check-sforms (state sforms vars)
  (let* ((fmla (make!-disjunction* (mapcar #'formula sforms)))
	 (result (ics-check state fmla vars)))
    (cond ((tc-eq fmla result)
	   (values 'X nil))
	  ((tc-eq result *true*)
           (values '? (make-instance 's-formula 'formula *true*)))
	  ((tc-eq result *false*)
	   (values '? (make-instance 's-formula 'formula *false*)))
	  (t
	   (when vars
	     (break "to do: printing of witnesses"))
	   (values 'X nil)))))


;; Querying ICS data base

(defstep query (&optional (context? t)
		          uninterp
			  arith
			  bool
			  tuple
			  diseqs
			  cnstrnts)
  (if (not (eql *current-decision-procedure* 'ics))
      (skip-msg "Querying only for ICS decision procedures")
    (let ((state (dp-state *ps*))
	  (bogus (ics-query state
			    :context? context?
			    :uninterp uninterp
			    :arith arith
			    :bool bool
			    :tuple tuple
			    :diseqs diseqs
			    :cnstrnts cnstrnts)))
      (skip)))
  "Querying ICS database. The proof state is left unchanged"
  "Querying ICS database")


;; Multiple instantiations

(defun insts-of-list (args &optional insts)
  "Construct a list of instantiations of the form (i . (t1 t2 ... tn))
   from a list of arguments. Returns nil if error occurs."
  (if (null args)
      (nreverse insts)
    (multiple-value-bind (fnum args1)      ; read fnum from argument list
	(scan-fnum-of-list args)
      (multiple-value-bind (terms args2)   ; read list of instantiation terms from argument list
	  (scan-terms-of-list args1)
	(when (and fnum terms)
	  (let ((new-insts (acons fnum terms insts)))
	    (insts-of-list args2 new-insts)))))))

(defun scan-fnum-of-list (args)
  (if (null args) nil
    (let ((arg1 (first args)))
      (cond ((or (integerp arg1) (symbolp arg1))
	     (values arg1 (rest args)))
	    ((and (consp arg1)
		  (or (integerp (first arg1))
		      (symbolp (first arg1))))
	     (let ((fnum (first arg1))
		   (terms (rest arg1)))
	       (values fnum (cons terms (rest args)))))))))

(defun scan-terms-of-list (args)
  (if (null args) nil
    (cond ((listp (first args))
	   (values (first args) (rest args)))
	  (t
	   (collect-terms args)))))

(defun collect-terms-of-list (args &optional terms)
  (if (null args)
      (values (nreverse terms) nil)
    (let ((arg1 (first args)))
      (if (or (integerp arg1) (symbolp arg1) (consp arg1))
	  (values (nreverse terms) args)
	(collect-terms-of-list (rest args) (cons arg1 terms))))))

(defun fnum-of-inst (inst)
  (when (consp inst) (first inst)))

(defun terms-of-inst (inst)
  (when (consp inst) (rest inst)))

(defun fmla-of-fnum (fnum)
  "Given a sequent index specifier FNUM, compute the
   first formula in the sequent of this index."
  (let* ((sforms (s-forms (current-goal *ps*)))
	 (sform (first (select-seq sforms fnum))))
    (when sform
      (formula sform))))

(defun fnum-of-fmla (fmla)
  "Get the FNUM of the formula FMLA in the current sequent."
  (fnum-of-fmla* fmla (s-forms (current-goal *ps*))))

(defun fnum-of-fmla* (fmla sforms &optional (posfnum 1) (negfnum -1))
  (if (null sforms) nil
    (let ((current-fmla (formula (first sforms))))
      (cond ((and (negation? current-fmla)
		  (tc-eq fmla current-fmla))
	     negfnum)
	    ((tc-eq fmla current-fmla)
	     posfnum)
	    (t
	     (multiple-value-bind (new-posfnum new-negfnum)
		 (if (negation? current-fmla)
		     (values posfnum (1- negfnum))
		   (values (1+ posfnum) negfnum))
	       (fnum-of-fmla* fmla (rest sforms) new-posfnum new-negfnum)))))))

(defun fnum-fmla-assoc (insts)
  "Association list between the FNUMs specified in the argument instantiations
   and the corresponding formulas in the current sequent. This association is
   needed, since  the FNUM for a formula may change; for example, by copying."
  (mapcar #'(lambda (inst)
	      (let ((fnum (fnum-of-inst inst)))
		(cons fnum (fmla-of-fnum fnum))))
	  insts))
		
(defstep inst* (&rest args)
  (let ((insts (insts-of-list args))       ;; normalize arguments
	(assocs (fnum-fmla-assoc insts)))
    (if nil
	(skip-msg "Insufficient instantiation")
      (inst*-loop assocs insts)))
  "Instantiates the top quantifiers by repeatedly applying
   INST or INST-CP if COPY? is nonnil. For example,
   (inst* -2 \"x+1\" \"y\" -3 \"y\" \"_\" \"3\") instantiates
   the top-level existential-strength quantifier of
   formula -2 with \"x+1\" and \"y\", respectively, and
   the first and third existentially quantified variable
   of formula -3 is instantiated with \"y\" and \"3\", respectively;
   the second quantification is retained."
  "Instantiating top quantifiers")

(defstep inst-cp* (&rest args)
   (let ((insts (insts-of-list args))       ;; normalize arguments
	(assocs (fnum-fmla-assoc insts)))   ;; fnums might change because of copying
    (if nil                                 ;; for repeated FNUMS in instantiations
	(skip-msg "Insufficient instantiation")
      (inst*-loop assocs insts :copy? t)))
  "Repeated instantiation like INST*, but instantiated formulas
   are copied by default."
  "Instantiating top quantifiers")

(defhelper inst*-loop (assocs insts &optional copy?)
  (if (null insts)
      (skip)
    (let ((inst1 (first insts))
	  (fnum1 (fnum-of-inst inst1))   ; fnum as originally specified
	  (terms1 (terms-of-inst inst1)) ; instantiations for top-level quantifiers
	  (requires-copy?                ; copy if there is another instantiation for same fnum
	   (some #'(lambda (inst)
		     (eql (fnum-of-inst inst) fnum1))
		 (rest insts))))
      (then
       (let ((real-fnum1                      ; fnum might have been changed by copying
	      (let ((real-fmla (cdr (assoc fnum1 assocs :test #'eql))))
		(if real-fmla 
		    (fnum-of-fmla real-fmla)
		  fnum1)))
	     (rule (if (or copy? requires-copy?)
		       `(inst-cp ,real-fnum1 ,@terms1)
	               `(inst ,real-fnum1 ,@terms1))))
	 rule)
       (let ((rest-insts (rest insts)))
	 (inst*-loop assocs rest-insts :copy? copy?)))))
  "The strategy used for defining INST*"
  "Trying repeated instantiation")
  

;; Heuristic instantiation

(defun generate-test-sequent (fmlas &optional bndngs antecedent succedent)
  "Put formulas NOT(FORALL x: p(x)) into antecedent and EXISTS y: q(x) in succedent,
   and accumulate bindings. For now, we assume bindings are distinct. Also, bindings
   should be translated to name-exprs."
  (if (null fmlas)
      (values antecedent succedent)
    (let ((fmla (first fmlas)))
      (multiple-value-bind (new-bndngs new-antecedent new-succedent)
	  (cond ((exists-expr? fmla)
		 (values (append (bindings fmla) bndngs) antecedent (cons fmla succedent)))
		((and (negation? fmla)
		      (forall-expr? (args1 fmla)))
		 (values (append (bindings fmla) bndngs) (cons fmla antecedent) succedent))
		(t
		 (values bndngs antecedent succedent)))
	(generate-test-fmla (rest fmlas) new-antecent new-succedent)))))

(defun solution-to-substs (solution)
  "A solution is an ICS list with bindings of the form 'x |-> {e1,...,en}'.
   This gives rise to n different substitutions depending on the binding for x."
  nil)
  
(defun generate-substs (state fmlas)
  (multiple-value-bind (vars antecedent succedent)
      (generate-test-sequent fmlas)
    (let ((expr (make!-implication
		 (make!-conjunction* antecedent)
		 (make!-disjunction* succedent))))
      (multiple-value-bind (result states)
	  (ics-check 'ics expr state)
	(declare (ignore result))
	(mapcan #'(lambda (state)
		    (solution-to-substs (ics-solution state vars)))
		states)))))
	
(defstep inst! (&optional (fnums *)
			  copy?
			  (relativize? t))
  (skip-msg "inst! does not work for now.")
  "" "")
