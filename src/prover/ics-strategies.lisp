(in-package :pvs)

;; Replace formula with a simplified version

(addrule 'sigmatize nil ((fnums *))
   (sigmatize-step fnums)
   "Replace formula with a simplified formula")

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


