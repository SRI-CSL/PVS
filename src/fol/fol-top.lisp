(in-package :pvs)

#-runtime

(addrule 'fol () ((fnums *) (iterations 3))
  (fol-step fnums iterations)
  "First-Order Proof Search.")

(defun fol-step (fnums iterations)
  #'(lambda (ps)
      (let* ((sforms (s-forms (current-goal ps)))
	     (selected-fmlas (mapcar #'formula
			        (select-seq sforms fnums)))
	     (remaining-sforms (delete-seq sforms fnums)))
	(let ((result (fol selected-fmlas iterations)))
	  (cond (result
		 (format-if "~%No proof found.")
	         (values 'X (current-goal ps)))
	        (t (format-if "~%Substitution: ~a" subst)
		   (values '? (list
			       (lcopy (current-goal ps)
				 's-forms remaining-sforms)))))))))
	   
(defun fol (fmlas iterations)
  (let* ((state (or ; *dp-state*
		    *init-dp-state*
		    (dp::null-single-cong-state)))
         (subst (dp::fol-search (herbrandize fmlas) state iterations)))
    (dp::fail? subst)))
