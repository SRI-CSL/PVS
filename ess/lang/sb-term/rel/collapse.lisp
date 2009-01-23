;;; -*- Mode: Lisp;  Package: syntax-box; Log: sb-changes.log  -*-
;;; Sccs Id @(#)collapse.lisp	1.7 11/2/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, Syntax Box.

;;; Anne Rogers 
;;; Revised, Scott Dietzen, Mon Oct 13 16:05:43 1986

;;;;    Basic Function: Collapse fragments into lisp functions

(in-package :syntax-box)  (use-package :ergolisp)


; The purpose of collapse is to take the fragments produced by process-grammar
; and produce lisp functions from them.  There are four kinds of functions 
; generated -- initials only, initials only with brackets, initials and medials,
; and initials and medials with brackets.


; Collapse-I-only: Number* x Fragment* x Number x Nt-name x Astract-syntax
;			x Look-ahead set* x boolean -> Lisp Function
;
;    Initials only, no brackets
;
; need-brackets will be true if the functions produced uses the bracket-list 
; argument.

(defun collapse-i-only (fragment-numbers fragment-list number-of-slots 
			nt nt-as fs-list need-brackets)
  (let* (branch-list fragment
             ; list of the main fragments.		    
	 (fragments
	  (mapcar #'(lambda (x)
		      (if (null (setq fragment
				      (assoc x fragment-list)))
			  (my-error 28)
			  fragment))
		  fragment-numbers))

             ; optimize the main fragment look-ahead sets.
	 (opt-las-list (optimize-las-list fragments)))
    
        ; for each main branch
    (do ((branches fragments (cdr branches)) (fragment-las))
	((null branches))
	    
      (setq fragment-las (cadr (assoc (get-number (car branches)) opt-las-list)))

         ; build branch entry ((la-match optimized-LAS)
	 ;		           collapsed parsing code)
      (push `((la-match ,fragment-las)
	      ,(peep-hole
		(collapse (get-code (car branches)) fragment-list)
		fragment-las))
	    branch-list))
    	
        ; catch errors.
    (push `(t (initial-error (quote ,fs-list))) branch-list)
    (setq branch-list (cons 'initials-only (nreverse branch-list)))

    (save-function
     `(defun ,(make-function-name nt)
	     (rbp &optional (bracket-list (empty-bracket-list)))
	(declare (ignore rbp))
	,(if (not need-brackets) '(declare (ignore bracket-list)))
	(let ,(make-let-variables number-of-slots)

	  ;; suppresses compiler warnings if available
	  (ergo-ignore-if-unused
	   ,@(make-let-variables number-of-slots))
	  ,branch-list
	  ,nt-as
	      ; final result is always in slot 0
	  ,(slot-code 0))))))



; Collapse-Bracket-I-only:  Number* x Fragment* x Number x Nt-name x 
;                             Astract-syntax x Look-ahead set*
;                                -> Lisp Function
;
;    Initials only, with brackets -- Except for the brackets this routine
;    is identical to collapse-I-only.

(defun collapse-bracket-i-only (fragment-numbers fragment-list number-of-slots 
						 nt nt-as fs-list gram-struct)

  (let* ((branch-list nil) (fragment nil)
	 (brackets (mapcar #'make-keyword
			   (reverse (get-nt-brackets nt gram-struct))))
					; revised srd
	 (leading-bracket (make-keyword (get-nt-left-bracket nt gram-struct)))
             ; get fragments for main branches
	 (fragments
	  (mapcar #'(lambda (x)
		      (if (null (setq fragment
				      (assoc x fragment-list)))
			  (my-error 28)
			  fragment))
		  fragment-numbers))

             ; optimize look ahead sets for main branches.
	 (opt-las-list (optimize-las-list fragments)))
    
        ; for each main branch
    (do ((branches fragments (cdr branches)) (fragment-las))
	((null branches))
	    
      (setq fragment-las (cadr (assoc (get-number (car branches)) opt-las-list)))

         ; build branch entry ((la-match optimized-LAS) 
	 ;		           collapsed parsing code)
      (push `((la-match ,fragment-las)
	      ,(peep-hole
		(collapse (get-code (car branches)) fragment-list)
		fragment-las))
	    branch-list))
    	
       ; catch errors -- do not want to fall out of the bottom.
    (push `(t (initial-error (quote ,fs-list))) branch-list)
    (setq branch-list (cons 'initials-only (nreverse branch-list)))

    (save-function
     `(defun ,(make-function-name nt)
	     (rbp &optional (bracket-list (empty-bracket-list)))
	(declare (ignore rbp))
	
	(let ,(make-let-variables number-of-slots)

	  ;; suppresses compiler warnings if available
	  (ergo-ignore-if-unused
	   ,@(make-let-variables number-of-slots))

	     ; keep track of all the leading brackets.
	  (stack-brackets ,leading-bracket ,nt)

	  ,branch-list
	  ,nt-as
	      ; eat all of the necessary closing brackets.
	  (eat-brackets ,brackets ,nt)

	      ; final result is always in slot 0
	  ,(slot-code 0))))))





; Build-LBP-code: produces code for finding LBP for medials.  For the jux
;  case, if the next token on the stream is in the first set of the
;  right sone of the jux, jux is the op, get its rbp.  Note: because peek-first
;  may not be the next op, we set a variable (mvar) to the next op.


(defun build-lbp-code (var-name regulars jux-branches lbps)
  (let ((code '((t 0))))

    (do ((juxes jux-branches (cdr juxes))
	 (temp nil))
	((null juxes))

      (if (atom (setq temp (make-medial-branch-las (nth 2 (car juxes)))))
	  (push `((eq (peek-first) (quote ,temp))
		  (setq ,var-name (quote ,(cadar juxes)))
		  ,(nth 3 (car juxes)))
		code)
	  (push `((member (peek-first) (quote ,temp))
		  (setq ,var-name (quote ,(cadar juxes)))
		  ,(nth 3 (car juxes)))
		code)))

					; srd: to prevent mistakes.  This may
					; not be the best place to do this.  
    (let ((undefined-ops (set-difference regulars
					 (mapcar #'(lambda (x)
						     (make-keyword (car x)))
						 lbps))))
      (if undefined-ops
	  (my-warning "Undefined operator kind and precedence for ~%  ~S.~%   ~
                      This may cause parsing problems."
		      undefined-ops))
      
      (push `((member (peek-first) (quote ,regulars))
	      (setq ,var-name (peek-first))
	      ,(if undefined-ops
		   `(cond ((cadr (assoc (peek-first)
					(quote ,(mapcar
						 #'(lambda (x)
						     (list (make-keyword (car x))
							   (cadr x)))
						 lbps)))))
			  (t 0))	; srd: so that missing precs do not
					; give an error. 
		   `(cadr (assoc (peek-first)
				 (quote ,(mapcar
					  #'(lambda (x)
					      (list (make-keyword (car x))
						    (cadr x)))
					  lbps))))))
	    code))

    (cons 'cond code)))


         
; Collapse-IM: Number* x Number* x Number x Fragment* x Nt-name x Abtract syntax x
;               Grammar Structure x Look ahead set* x Look ahead set* x boolean
;               jux-info* -> Lisp Function
;
;    Initials and Medials, no brackets.  Need-brackets will be true if the
;    function produced needs the bracket-list argument.

(defun collapse-im (initials medials number-of-slots fragment-list nt nt-as
			     gram-struct initial-fs-list medial-fs-list
			     need-brackets jux-branches)

  (let* ((init-list nil) (medial-list nil) (init-fragment nil)
	 (regular-ops nil) (lbp-code nil) (mvar 'mtemp)
	 (var-list (cons mvar (make-let-variables number-of-slots)))
	     ; left binding powers.
	 (lbps (get-nt-prec-medial-lefts nt gram-struct))
             ; initial branch fragments
	 (init-fragments
	  (mapcar #'(lambda (x)
		      (if (null (setq init-fragment
				      (assoc x fragment-list)))
			  (my-error 28)
			  init-fragment))
		  initials))
             ; optimize initial look ahead sets
	 (opt-las-list (optimize-las-list init-fragments)))
    
         ; for each initial branch (This is identical to the code used
					 ; for collapse-I-only)
    (do ((branches init-fragments (cdr branches)) (fragment-las))
	((null branches))
	    
	    
      (setq fragment-las (cadr (assoc (get-number (car branches)) opt-las-list)))

          ; save initial branch code
      (push `((la-match ,fragment-las)
	      ,(peep-hole
		(collapse (get-code (car branches)) fragment-list)
		fragment-las))
	    init-list))

    	
       ; catch errors
    (push `(t (initial-error (quote ,initial-fs-list))) init-list)
    (setq init-list (cons 'initials (nreverse init-list)))


        ; for each medial
    (do ((meds medials (cdr meds)) (jux-value) (fragment))
	((null meds))
	     
          ; get the fragment
      (setq fragment (assoc (car meds) fragment-list))
		   
      (cond ((null fragment) (my-error 28))

	    ((setq jux-value (assoc (get-number fragment) jux-branches))
	     (push (list (cadr jux-value)
			 (collapse (get-code fragment) fragment-list))
		   medial-list))

	    (t
	         ; add an entry for the medial case statement
	         ; Medials only have one symbol of look ahead.
	     (let ((medial-fragment-las
		    (make-medial-branch-las (get-fs fragment))))

	       (if (atom medial-fragment-las)
		   (setq regular-ops (union (list medial-fragment-las)
					    regular-ops))
		   (setq regular-ops (union medial-fragment-las regular-ops)))


	       (push (list medial-fragment-las
			   (medial-peep-hole
			    (collapse (get-code fragment) fragment-list)
			    medial-fragment-las))
		     medial-list)))))

    (push `(t (medial-error (quote ,medial-fs-list))) medial-list)
    (setq medial-list (append `(case ,mvar) (nreverse medial-list)))
	 

    (setq lbp-code (build-lbp-code mvar regular-ops jux-branches lbps))

    (save-function
     `(defun ,(make-function-name nt)
	     (rbp &optional (bracket-list (empty-bracket-list)))

	,(if (not need-brackets) '(declare (ignore bracket-list)))
	(let ,var-list

	  ;; suppresses compiler warnings if available
	  (ergo-ignore-if-unused
	   ,@(make-let-variables number-of-slots))

	  ,init-list
	  ,nt-as
	 
	     ; pick up as many medials as possible -- as long as
	     ; LBP(next token) > rbp.

	  (cond ((> ,lbp-code rbp)
		 (loop
		     ; set all the variables to nil.
		  (clean-variables ,(cddr var-list))
		  ,medial-list
		      ; each time through we need to do the abstract syntax
		  ,nt-as
		  (when (<= ,lbp-code rbp) (return nil)))))
		   
	      ; final result is always in slot 0
	  ,(slot-code 0))))))




; Make-medial-branch-LAS: look-ahead-set -> symbol*
;   Make list of first symbols -- used as the key for the medial case statement.

(defun make-medial-branch-las (fs-list)
  (let ((result (mapcar #'car fs-list)))
    (if (= (length result) 1) (car result) result)))





; Collapse-bracket-IM: Number* x Number* x Number x Fragment* x Nt-name
;                       x Abtract syntax x Grammar Structure x Look ahead set*
;                       x Look ahead set* x jux-info* -> Lisp Function.
;
;    Initials and Medials, with brackets.  Same as Collapse except for
;     bracket stuff.

(defun collapse-bracket-im (initials medials number-of-slots fragment-list nt
			    nt-as gram-struct initial-fs-list medial-fs-list
			    jux-branches)

  (let* ((init-list nil) (init-fragment nil) (medial-list nil)
	 (mvar 'mtemp) (lbp-code nil) (regular-ops nil)
	 (var-list (cons mvar (make-let-variables number-of-slots)))
             ; left binding powers
	 (lbps (get-nt-prec-medial-lefts nt gram-struct))
	 (brackets (mapcar #'make-keyword
			   (reverse (get-nt-brackets nt gram-struct))))
	 (leading-bracket (make-keyword (get-nt-left-bracket nt gram-struct)))
					; revised srd, list of main initial
					; fragments.
	 (init-fragments
	  (mapcar #'(lambda (x)
		      (if (null (setq init-fragment
				      (assoc x fragment-list)))
			  (my-error 28)
			  init-fragment))
		  initials))
             ; optimize initial look ahead sets
	 (opt-las-list (optimize-las-list init-fragments)))
    
        ; for each initial branch
    (do ((branches init-fragments (cdr branches)) (fragment-las))
	((null branches))
	    
      (setq fragment-las
	    (cadr (assoc (get-number (car branches)) opt-las-list)))

          ; save initial branch code
      (push `((la-match ,fragment-las)
	      ,(peep-hole
		(collapse (get-code (car branches)) fragment-list)
		fragment-las))
	    init-list))


        ; catch errors
    (push `(t (initial-error (quote ,initial-fs-list))) init-list)
    (setq init-list (cons 'initials (nreverse init-list)))

        ; for each medial
    (do ((meds medials (cdr meds)) (jux-value) (fragment))
	((null meds))
	     

          ; get fragment
      (setq fragment (assoc (car meds) fragment-list))

      (cond ((null fragment) (my-error 28))

	    ((setq jux-value (assoc (get-number fragment) jux-branches))
	     (push (list (cadr jux-value)
			 (collapse (get-code fragment) fragment-list))
		   medial-list))

	    (t
	         ; build entry for branch in the medial case statement.
	     (let ((medial-fragment-las
		    (make-medial-branch-las (get-fs fragment))))

	       (if (atom medial-fragment-las)
		   (setq regular-ops (union (list medial-fragment-las)
					    regular-ops))
		   (setq regular-ops (union medial-fragment-las regular-ops)))


	       (push (list medial-fragment-las
			   (medial-peep-hole
			    (collapse (get-code fragment) fragment-list)
			    medial-fragment-las))
		     medial-list)))))
    
        ; catch errors
    (push `(t (medial-error (quote ,medial-fs-list))) medial-list)
    (setq medial-list (append `(case ,mvar) (nreverse medial-list)))
	 

    (setq lbp-code (build-lbp-code mvar regular-ops jux-branches lbps))
    
    (save-function
     `(defun ,(make-function-name nt)
	     (rbp &optional (bracket-list (empty-bracket-list)))


	(let ,var-list

	  ;; suppresses compiler warnings if available
	  (ergo-ignore-if-unused
	   ,@(make-let-variables number-of-slots))

	     ; keep track of all leading brackets.
	  (stack-brackets ,leading-bracket ,nt)
	  
	  ,init-list
	  ,nt-as

	     ; eat as many brackets as necessary.
	  (eat-brackets ,brackets ,nt)

	  (cond ((> ,lbp-code rbp)
		 (loop
		     ; set all variables to nil
		  (clean-variables ,(cddr var-list))
		  ,medial-list
		     ; do the nt abstract syntax again
		  ,nt-as
		     ; eat as many brackets as necessary
		  (eat-brackets ,brackets ,nt)
		  (when (<= ,lbp-code rbp) (return nil)))))
		   
	     ; final result is always in slot 0
	  ,(slot-code 0))))))



; Collapse: Code x Fragment*  -> Code
(defun collapse (code fragment-list)

  (cond ((atom code) code)
	(t (case (car code)
	         ; (opt-parse LAS code)
	     (opt-parse 
	      (let ((fragment (assoc (nth 2 code) fragment-list)))

		(if (null fragment) (my-error 28))
		`(opt-parse ,(get-fs fragment)
			    ,(peep-hole
			      (collapse (get-code fragment) fragment-list)
			      (get-fs fragment))
			    ,(slot-code (cadr code)))))

	         ; (alt-parse ((la-match LAS) code)*)
	         ; we do a branch look ahead optmization just like the
	         ; ones we do in collapse-I-only.
	     (alt-parse
	      (let* ((fragment nil) (clause-list nil) (error-list nil)
		         ; list of main branch fragments
		     (branch-fragments
		      (mapcar #'(lambda (x)
				  (if (null (setq fragment
						  (assoc (car x)
                                                         fragment-list)))
				      (my-error 28)
				      fragment))
			      (cadr code)))
                         ; optimize branch look ahead sets.
		     (opt-las-list (optimize-las-list branch-fragments)))
		     
		    ; for each branch
		(do ((fragments branch-fragments (cdr fragments))
		     (branch-number) (fragment-las))
		    ((null fragments))
			
		  (push (get-fs (car fragments)) error-list)
		  
		  (setq fragment-las
			(cadr (assoc (get-number (car fragments))
                                     opt-las-list)))

		  (setq branch-number
			(cadr (assoc (get-number (car fragments)) (cadr code))))

		      ; build branch entry
		  (push `((la-match ,fragment-las)
			      ; set flag slot to the correct branch number
			  (value-to-slot ,(slot-code (nth 2 code))
                                         ,branch-number)
			      ; parsing code
			  ,(peep-hole
			    (collapse (get-code (car fragments)) fragment-list)
			    fragment-las))
			clause-list))

		    ; catch erros    
		(push `(t (initial-error (quote ,(compare-fs-list error-list))))
		      clause-list)
		(cons 'alt-parse (reverse clause-list))))


             
             ;; (star-parse LAS code final-result partial-result)
	     (star-parse
	      (let ((fragment (assoc (cadr code) fragment-list)))
		
		(if (null fragment) (my-error 28))
		`(star-parse ,(get-fs fragment)
			     ,(peep-hole
			       (collapse (get-code fragment) fragment-list)
			       (get-fs fragment))
			     ,(slot-code (nth 2 code))
			     ,(slot-code (nth 3 code)))))
             
             ;; (gen-star-parse LAS code final-result partial-result)
	     (gen-star-parse
	      (let ((fragment (assoc (cadr code) fragment-list)))
		
		(if (null fragment) (my-error 28))
		`(gen-star-parse ,(get-fs fragment)
				 ,(peep-hole
				   (collapse (get-code fragment) fragment-list)
				   (get-fs fragment))
				 ,(slot-code (nth 2 code))
				 ,(slot-code (nth 3 code)))))
             
             ;; (plus-parse LAS code final-result partial-result)
	     (plus-parse
	      (let ((fragment (assoc (cadr code) fragment-list)))
		 
		(if (null fragment) (my-error 28))
		`(plus-parse ,(get-fs fragment)
			     ,(peep-hole
			       (collapse (get-code fragment) fragment-list)
			       (get-fs fragment))
			     ,(slot-code (nth 2 code))
			     ,(slot-code (nth 3 code)))))
	       
             
             ;; (doublestar-parse LAS code final-result partial-result
             ;;                   separator)
	     (doublestar-parse
	      (let ((fragment (assoc (cadr code) fragment-list)))
			 
		(if (null fragment) (my-error 28))
		`(doublestar-parse ,(get-fs fragment)
				  ,(peep-hole
				    (collapse (get-code fragment) fragment-list)
				    (get-fs fragment))
				  ,(slot-code (nth 2 code))
				  ,(slot-code (nth 3 code))
				  ,(nth 4 code))))
                

	         ;; (doublestar-parse LAS code final-result partial-result
                 ;;                       separator)
	     (doubleplus-parse
	      (let ((fragment (assoc (cadr code) fragment-list)))
			 
		(if (null fragment) (my-error 28))
		`(doubleplus-parse ,(get-fs fragment)
				  ,(peep-hole
				    (collapse (get-code fragment) fragment-list)
				    (get-fs fragment))
				  ,(slot-code (nth 2 code))
				  ,(slot-code (nth 3 code))
				  ,(nth 4 code))))


	        ; collapse code inside the prog.  get rid extra progs.
	     (progn
	      (let (clause-list temp)
		    
		(do ((pieces (cdr code) (cdr pieces)))
		    ((null pieces))
		  (setq temp (collapse (car pieces) fragment-list))
		  (cond ((eq (car temp) 'progn)
			 (setq clause-list
			       (append (reverse (cdr temp)) clause-list)))
			(t (push temp clause-list))))
		      
		(if (= (length clause-list) 1)
		    (car clause-list)
		    (cons 'progn (nreverse clause-list)))))

	        ; return collapse of the fragment pointed to by the parse.
	     (parse
	      (let ((fragment (assoc (cadr code) fragment-list)))
		    
		(if (null fragment) (my-error 28))
		(collapse (get-code fragment) fragment-list)))

	     (setf
	      (if (and (eq (car (cadr code)) 'aref)
		       (eq (car (caddr code)) 'aref))
		  `(slot-to-slot ,(nth 2 (cadr code)) ,(nth 2 (caddr code)))
		  `(code-to-slot ,(nth 2 (cadr code)) 
				 ,(collapse (nth 2 code) fragment-list))))
					
	     (t code)))))



; Optimize-LAS-list: (Number x look-ahead set)*  -> (number x Look-ahead set)*
;    Optmize look ahead sets.  If we only need one symbol to choose a
;     production then throw away the seconds.  This means that if the leading
;     symbol in an entry is not used anywhere else then we do not need the seconds
;     from that entry.  To check this we build a composite look ahead set from all
;     of the look ahead sets and then check each set against this new one.

(defun optimize-las-list (fragment-list)

	    ; composite look ahead set.
  (let ((full-las (compare-fs-list (mapcar #'(lambda (x) (get-fs x)) fragment-list))))
	
       ; for each (number x look ahead set)
    (do ((fragments fragment-list (cdr fragments)) (new-las nil nil) (result))
	((null fragments) result)

          ; for each entry in Look ahead set
      (do ((las-entries (get-fs (car fragments)) (cdr las-entries)) (temp))
	  ((null las-entries))

	    ; get the entry for this leading symbols from the composite
	    ; look ahead set.
	(setq temp (assoc (caar las-entries) full-las))

	    ; if the seconds list for the entry is not empty then check to see
	    ; if there are any extra symbols in the seconds list from the
	    ; composite LAS.  If there are entra symbols the we need both
	    ; to pick a branch.  If not then we can throw away the seconds.
	(if (cdar las-entries)
	    (if (eq (length (cdr temp)) (length (cdar las-entries)))
		(push (list (caar las-entries)) new-las)
		(push (car las-entries) new-las))
	    (push (car las-entries) new-las)))

          ; save (Number x New look ahead set) in result.
      (push (list (get-number (car fragments)) new-las) result))))



; Make-Let-variables: Number -> Variable*
;  Make N-1 Variables, where a variable looks like V5.

(defun make-let-variables (n)
   (let (vars)

	(do ((i 0 (+ i 1)))
	    ((= i n) (reverse vars))

	  (push (intern (format nil "~A~D" :v i) *sb-package*)
		vars))))


; Peep-hole
;   hook for adding optimizations
;

(defun peep-hole (code look-ahead-set)
  (declare (ignore look-ahead-set))
  code)

   ; medial look ahead sets do not look the same as regular ones
   ; make them look like regular before calling peep-hole
(defun medial-peep-hole (code medial-look-ahead-set)
  (declare (ignore medial-look-ahead-set))
  code)


