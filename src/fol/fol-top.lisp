(in-package :pvs)

(addrule 'fol () ((fnums *) (iterations 27))
  (fol-step fnums iterations)
  "First-Order Proof Search.")

(defun fol-step (fnums iterations)
  #'(lambda (ps)
      (let* ((sforms (s-forms (current-goal ps)))
	     (selected-fmlas (mapcar #'formula
			        (select-seq sforms fnums)))
	     (remaining-sforms (delete-seq sforms fnums)))
	(let ((subst (fol selected-fmlas iterations *init-dp-state*)))
	  (if (dp::fail? subst)
	      (progn
		 (error-format-if "~%No proof found.")
	         (values 'X (current-goal ps)))
	    (progn
	      (error-format-if "~%Substitution: ~a" subst)
	      (values '? (list
			   (lcopy (current-goal ps)
			          's-forms remaining-sforms)))))))))
	   
(defun fol (fmlas &optional (iterations 0)
			    (state *init-dp-state*)
		            (k #'identity))
  #+dbg(assert (not (null *init-dp-state*)))
  (let ((dp::*iterations* iterations)
	(dp::*init-dp-state* *init-dp-state*))
    (declare (special dp::*iterations*)
	     (special dp::*init-dp-state*))
    (dp::fol-search state (herbrandize fmlas) k)))
