;;; -*- Mode: Lisp;  Package: syntax-box; Log: sb-changes.log  -*-
;;; Sccs Id @(#)unparse-gen.lisp	1.54 2/12/90
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;



;;; Ergo Syntax Box

;;; This is the unparser generator written by Scott Dietzen.

(in-package :syntax-box)  (use-package :ergolisp)


(eval-when (eval compile load)

(defmacro map-append (function &rest lists)
  `(do ((list-to-append
	 ,(append `(mapcar ,function)
		  lists)
	 (cdr list-to-append))
	(result () (append result (car list-to-append))))
       ((null list-to-append) result)))

(defmacro var (n)
  `(sb-intern-nupcase (concatenate 'string "V" (prin1-to-string ,n)))))



(defvar *unparse-gen-debug* ())

(defvar *nt-name* ())

(defvar *top-level-alts* ())

;;;    @Hack: Documented in inter-phase.

(defparameter mid-double-rept-bp-factor 2
  "Constant used to discourage breaking before a doubleplus or doublestar
   keyword.")

(defparameter unite-double-rept nil
  "Should doublestar and doubleplus be united by default when the keyword is
   not ','.")
  ;; Above nil because the formatting language makes it easy to unite, but
  ;; there is no way to ununite.

(defparameter unite-double-rept-commas nil
  "Should doublestar and doubleplus be united by default when the keyword 
   is ','.")
  ;; Since usually you do not want to unite with a comma separator, this is
  ;; provided separately. 



(defun mk-conc-var-name (string &key (lg nil))
  (let* ((lg (or lg *language*))
	 (conc-name (lang:lang-conc-name lg)))
    (sb-intern-nupcase
     (string-upcase (concatenate 'string
		      conc-name
		      "-"
		      string)))))


(eval-when (compile eval load)

(defmacro mk-bracket-table-name ()
  `(mk-conc-var-name "BRACKET-INFO"))

(defmacro mk-prec-table-name ()
  `(mk-conc-var-name "PREC-INFO"))

(defmacro mk-spacing-table-name ()
  `(mk-conc-var-name "SPACING-INFO"))

(defmacro mk-key-token-table-name ()
  `(mk-conc-var-name "KEY-TOKEN-MAP"))

(defmacro mk-key-esc-token-table-name ()
  `(mk-conc-var-name "KEY-ESC-TOKEN-MAP"))

(defmacro mk-lt-token-table-name ()
  `(mk-conc-var-name "LT-TOKEN-MAP"))

(defmacro mk-lt-esc-token-table-name ()
  `(mk-conc-var-name "LT-ESC-TOKEN-MAP"))

(defmacro mk-unparse-name ()
  `(lang:lang-unparse-routine-name *language*))

(defmacro mk-win-unparse-name ()
  `(lang:lang-win-unparse-routine-name *language*))

)   ; eval-when




(defun mk-unparse-routine-name (nt-name &key (lg-name nil))
  (mk-conc-var-name (concatenate 'simple-string
		      "UNP-"
		      (symbol-name nt-name))
		    :lg (if lg-name
			    (lang:coerce-find-lang lg-name))))

(defun mk-aux-unparse-routine-name (nt-name &key (lg-name nil))
  (mk-conc-var-name (concatenate 'simple-string
		      "UNP-"
		      (symbol-name nt-name)
		      "-AUX")
		    :lg (if lg-name
			    (lang:coerce-find-lang lg-name))))

(defun mk-discrim-routine-name (nt-name &key (lg-name nil))
  (mk-conc-var-name (concatenate 'string
		      "DIS-"
		      (symbol-name nt-name))
		    :lg (if lg-name
			    (lang:coerce-find-lang lg-name))))







(defun generate-unparser (grammar nts-to-unparse file-spec)
  (setq *unparser-gen-error* nil)
  (clet* ((*grammar-term* grammar)      ;; defined elsewhere, but no problem.
	  (output-file (open file-spec
			     :direction :output :if-exists :new-version)
		       (close output-file)))
    (format t "   writing file ~S.~%" file-spec)
    (initialize-file output-file :unparser)
    (output-initial-unparsing-info grammar output-file)
    (format output-file "~%~|")
    (sb-write (pprint (gen-nt-unparse-routine)
		      output-file))
    (format output-file "~%~|")
    (sb-write (pprint (gen-uterm-format-routine)
		      output-file))
    (generate-nt-routines nts-to-unparse output-file)    
    (terpri output-file)))



(defun output-initial-unparsing-info (grammar output-file)
  (output-bracket-info grammar output-file)
  (terpri output-file)
  (output-precedence-info grammar output-file)
  (terpri output-file)
  (output-spacing-info grammar output-file)
  (terpri output-file)
  (output-token-map-info output-file)
  (terpri output-file))



(defun output-bracket-info (grammar output-file)
  (let ((table-name (mk-bracket-table-name)))
    (sb-write
     (pprint `(defvar ,table-name (make-hash-table :test #'eq))
	     output-file)
     (pprint `(clrhash ,table-name) output-file)
     (pprint
      `(mapc #'(lambda (entry)
		  (set-bracket-info (car entry)
				    (cadr entry)
				    (caddr entry)
				    ,table-name))
	     ',(mapcan #'(lambda (nt-entry)
			   (let ((brackets
				  (get-nt-brackets (nt-name nt-entry) grammar)))
			     (if brackets 
				 (list (list (nt-name-abs-package nt-entry)
					     (car brackets)
					     (cadr brackets))))))
		       (grammar-nonterminal-definitions grammar)))
      output-file))))



(defun output-precedence-info (grammar output-file)
  (let ((table-name (mk-prec-table-name)))
    (sb-write
     (pprint `(defvar ,table-name (make-hash-table :test #'eq))
	     output-file)
     (pprint `(clrhash ,table-name) output-file)
     (pprint
      `(mapc #'(lambda (nt)
		 (init-prec-info nt ,table-name))
	     ',(mapcan #'(lambda (nt-entry)
			   (cond ((get-nt-prec (nt-name nt-entry) grammar)
				  (list (nt-name-abs-package nt-entry)))))
		       (grammar-nonterminal-definitions grammar)))
      output-file))

    (let ((result ()))
      (mapc
       #'(lambda (nt-entry)
	   (let ((prec-info (get-nt-prec (nt-name nt-entry) grammar)))
	     (cond (prec-info
		    (setq result
			  (append 
			   result
			   (write-prec-entries (nth 0 prec-info)
					       (nt-name-abs-package nt-entry)
					       0)
			   (write-prec-entries (nth 1 prec-info)
					       (nt-name-abs-package nt-entry)
					       1)
			   (write-prec-entries (nth 2 prec-info)
					       (nt-name-abs-package nt-entry)
					       2)
			   (write-prec-entries (nth 3 prec-info)
					       (nt-name-abs-package nt-entry)
					       3)))))))
       (grammar-nonterminal-definitions grammar))

      (sb-write
       (pprint
	`(mapc #'(lambda (entry)
		   (set-prec-info (car entry)
				  (cadr entry)
				  (caddr entry)
				  (cadddr entry)
				  ,table-name))
	       ',result)
        output-file)))))



(defun write-prec-entries (prec-entries nt-name table-num)
  (mapcar #'(lambda (prec-entry)
	      (list nt-name
		    (car prec-entry)
		    table-num
		    (cadr prec-entry)))
	  prec-entries))





(eval-when (compile eval load)
(defmacro keyword-intern (x)
  `(let ((x ,x))
     (intern (if (stringp x)
		 x 
		 (symbol-name x))
	     (find-package :keyword)))))


(defun output-spacing-info (grammar output-file)
  (let ((spacing-table-name (mk-spacing-table-name)))
    (sb-write
     (pprint `(defvar ,spacing-table-name (make-hash-table :test #'eq))
	     output-file)
     (pprint `(clrhash ,spacing-table-name) output-file))
    (do ((clauses (grammar-spacing grammar)
		  (cdr clauses))
	 (t1s nil
	      (cons (get-spacing-symbol (term-arg0 (car clauses)))
		    t1s))
	 (sps nil
	      (cons (let ((bp-spec (make-bp-spec (term-arg1 (car clauses)))))
		      (cond ((nth 8 bp-spec)	; crs field
			     (- (nth 8 bp-spec)))    
					; convention, see rt-format.lisp avoids
					; passing structures.
			    ((nth 6 bp-spec)))) ; spaces field
		    sps))
	 (t2s nil
	      (cons (get-spacing-symbol (term-arg2 (car clauses)))
		    t2s)))
	((null clauses)
	 (sb-write
	  (pprint
	   `(mapc #'(lambda (t1 t2 bp)
		      (cond ((assoc t2 (gethash t1 ,spacing-table-name)))
					; already set
			    (t
			     (setf (gethash t1 ,spacing-table-name)
				   (cons (list t2 bp)
					 (gethash t1 ,spacing-table-name))))))
		  ,@(setup-spacing-table (nreverse t1s)
					 (nreverse t2s)
					 (nreverse sps)))
	   output-file))))))


(defun get-spacing-symbol (term)
  (cond ((is-keyword term)
	 (sbst-intern-ncase (ds-keyword term)))
	((and (is-id term)
	      (memq (ds-id term)
		    '(arb op lt)))
	 (keyword-intern (ds-literal term)))
	((and (is-id term)
	      (is-lexical-terminal (sb-intern-case (ds-id term))
				   *grammar-term*))
	 (sbst-intern-case (ds-id term)))
	((is-id term)			; NT
	 (ds-id term))
	((eq (sim-term-op term) 'jux-op)
	 (sbst-intern-upcase (if (is-sop 'null
					 (term-arg0 term))
				 'jux 
				 (mk-jux-name (term-arg0 term)))))))



(defun setup-spacing-table (t1s t2s bps)
  (let ((new-t1s nil)
	(new-t2s nil)
	(new-bps nil))
    (do ((t1s t1s (cdr t1s))
	 (t2s t2s (cdr t2s))
	 (bps bps (cdr bps)))
	((null t1s)
	 `(',(nreverse new-t1s)
	   ',(nreverse new-t2s)
	   ',(nreverse new-bps)))
         ;; The following are separate cond branches so that in the case of
	 ;; :arb the proper thing will happen.  Otherwise they are disjoint. 
      (cond ((not (eq (symbol-package (car t1s))
		      (find-package 'keyword)))
	     (push (car t1s) new-t1s)
	     (push (car t2s) new-t2s)
	     (push (car bps) new-bps)))
      (cond ((or (eq (car t1s) :op)    
		 (eq (car t1s) :arb))
	     (mapc #'(lambda (t1)
		       (let ((t1 (sbst-intern-case t1)))
			 (push t1 new-t1s)
			 (push (car t2s) new-t2s)
			 (push (car bps) new-bps)))
		   (grammar-operator-list *grammar-term*))))
      (cond ((or (eq (car t1s) :lt)
		 (eq (car t1s) :arb))
	     (mapc #'(lambda (t1)
		       (let ((t1 (sbst-intern-case t1)))
			 (push t1 new-t1s)
			 (push (car t2s) new-t2s)
			 (push (car bps) new-bps)))
		   (grammar-lexical-terminals *grammar-term*))))
      (cond ((eq (car t1s) :arb)
	     (mapc #'(lambda (t1)
		       (push t1 new-t1s)
		       (push (car t2s) new-t2s)
		       (push (car bps) new-bps))
		   (grammar-nt-names *grammar-term*)))))))
	     



(defun output-token-map-info (output-file)
  (let ((key-table-name (mk-key-token-table-name))
	(key-esc-table-name (mk-key-esc-token-table-name))
	(lt-table-name (mk-lt-token-table-name))
	(lt-esc-table-name (mk-lt-esc-token-table-name)))
    (sb-write
     (pprint `(defvar ,key-table-name (make-hash-table :test #'eq))
	     output-file)
     (pprint `(defvar ,key-esc-table-name
		      (if ,*conc-escape-char*
			  (make-hash-table :test #'eq)))
	     output-file)
     (pprint `(defvar ,lt-table-name (make-hash-table :test #'equal))
	     output-file)
     (pprint `(defvar ,lt-esc-table-name 
		      (if ,*conc-escape-char*
			  (make-hash-table :test #'equal)))
	     output-file))))




(defun gen-nt-unparse-routine ()
  `(defun ,(mk-unparse-name) (nt as &key (style *unparse-style*))
     (let* ((*unparser-op-list* ,*conc-all-operators-list*)
	    (*key-token-map* ,(mk-key-token-table-name))
	    (*key-esc-token-map* ,(mk-key-esc-token-table-name))
	    (*lt-token-map* ,(mk-lt-token-table-name))
	    (*lt-esc-token-map* ,(mk-lt-esc-token-table-name))
	    (*apply-lt-dis-fun* #'apply-lexical-terminal-discriminator)
	    (*apply-lt-des-fun* #'apply-lexical-terminal-destructor)
	    (*apply-lt-token-cons-fun* #'apply-lt-token-constructor)
	    (*bracket-info* ,(mk-bracket-table-name))
	    (*prec-info* ,(mk-prec-table-name))
	    (sort (sort:opsig-output
		   (sort:opsig-table-lookup (term:term-op as))))
	    (nt-name (cond (nt)
			   ((sort:is-sort-ttype sort)
			    (sort:ds-sort-ttype sort))
			   (t 
			    ',(nt-name-abs-package
			       (car (grammar-nonterminal-definitions
				     *grammar-term*))))))

	    (*case-sensitive* ,*conc-case-sensitive*)
	    (*escape-character* ,*conc-escape-char*)
	    (*restricted-chars* ,*conc-restricted-chars*)
	    (*string-char* ,*conc-string-char*)
	    (*literal-char* ,*conc-literal-char*)
	    (*keyword-char* ,*conc-keyword-char*)
	    (*unparse-style* (if (and style
				      (not (consp style)))
				 (list style)
				 style))
	    (*no-escapes* (or *no-escapes*
			      (null (insert-escapes?))))
	    (*current-print-depth* 1))

       (let ((uterm
	      (case nt-name
		    ,@(do ((runner (grammar-nonterminal-definitions
				    *grammar-term*)
 				   (cdr runner))
			   (result ()
				   (cons
				    `(,(nt-name-abs-package (car runner))
				      (,(mk-unparse-routine-name
					 (nt-name (car runner)))
				       as
				       :top-level? t))
				    result)))
			  ((null runner)
			   (reverse
			    (cons `(t (unparse-runtime-error
				       "nonterminal not unparsable:"
				       nt-name nil)
				      ());; return nil to avoid mapping above.
				  result)))))))
	    uterm))))



(defun gen-uterm-format-routine ()
  `(defun ,(mk-win-unparse-name) (uterm width
					&key 
					(fontwidth sbrt::sb-deffontwidth)
					(fontheight sbrt::sb-deffontheight))
     (let* ((*case-sensitive* ,*conc-case-sensitive*)
	    (*unparser-op-list* ,*conc-all-operators-list*)
	    (*spacing-info* ,(mk-spacing-table-name)))
       (format-uterm uterm width :fontwidth fontwidth
		                 :fontheight fontheight))))
	   




(defun generate-nt-routines (nts-to-unparse output-file)
  (let ((nt-attr-routines nil)
	(nt-unp-routines nil)
	(nt-dis-routines nil))
    (do ((nt-runner nts-to-unparse (cdr nt-runner)))
	((null nt-runner))
      (setf *nt-name* (nt-name (car nt-runner)))
      (if *unparse-gen-debug*
	  (format t "   ~A ... ~%" *nt-name*)) ; progress report
      (push (gen-nt-attr (car nt-runner))
	    nt-attr-routines)
      (push (gen-nt-unparser (car nt-runner))
	    nt-unp-routines)
      (push (gen-nt-discriminator (car nt-runner))
	    nt-dis-routines))
    (setq nt-attr-routines (nreverse nt-attr-routines))
    (setq nt-unp-routines (nreverse nt-unp-routines))
    (setq nt-dis-routines (nreverse nt-dis-routines))
    
    (setq nt-unp-routines (unp-code-revision nt-unp-routines))
    
    (do ((routine-runner (nconc nt-attr-routines
				nt-unp-routines
				nt-dis-routines)
			 (cdr routine-runner)))
	((null routine-runner))
      (format output-file "~%~%~|~%")
      (sb-write (pprint (car routine-runner)
			output-file)))
    (terpri output-file)))
    
    


(defun gen-nt-attr (nt)
  (let ((nt-name (nt-name nt)))
    `(defun ,(mk-unparse-routine-name nt-name) (as
						&key (top-level? nil))
       (memo-uterm as #',(mk-aux-unparse-routine-name nt-name)
		   :top-level? top-level?))))




(defun gen-nt-unparser (nt)
  (let ((nt-name (nt-name-abs-package nt))
	(nt-pat (nt-pattern nt)))
    `(defun ,(mk-aux-unparse-routine-name nt-name) (as)
	    (let (,@(do ((i (1- (get-nt-slot-total nt)) (1- i))
			 (result () (cons (var i) result)))
			((= i -1) result)))
		 ;; suppresses compiler warnings if available
		 (ergo-ignore-if-unused 
		  ,@(do ((i (1- (get-nt-slot-total nt)) (1- i))
			 (result () (cons (var i) result)))
			((= i -1) result)))
		 (setf ,(var (cond ((and (memq (pattern-kind nt-pat)
					       '(alt opt))
					 (not (memq nt-pat
						    *top-level-alts*)))
				    ;; @hack, documented in inter-phase
				    (+ 1 (get-pattern-slot nt-pat)))
				   (t 
				    (get-pattern-slot nt-pat))))
		       as)
		 (nt-unp
		  ',nt-name
		  as
		   ,(gen-pattern-&-upattern-unparser nt-pat))))))




(defun gen-pattern-&-upattern-unparser (pat)
  (if (null (pattern-upats pat))
      (gen-pattern-&-augment-unparser pat)
      `((upats
	 ,@(let ((parse-code `((:parse) ,@(gen-pattern-&-augment-unparser pat)))
		 (branches nil)
		 (default-code nil))
	     (do ((upats (pattern-upats pat)
			 (cdr upats)))
		 ((null upats)
		  (if default-code
		      (cons parse-code
			    (nreverse (cons default-code branches)))
		      (nreverse (cons `(t ,@(cdr parse-code))
				      branches))))
	       (if (or (null (upat-uids (car upats)))
		       (member :default (upat-uids (car upats)) :test #'eq))
		   (if default-code
		       (unparse-gentime-error 
			(format nil " Warning: Multiple default unparsing ~
			   patterns.  Ignoring additional ones.~%")
			nil)
		       (setq default-code
			     `(t ,@(gen-pattern-&-augment-unparser
				    (upat-pattern (car upats))))))
		   (setq branches
			 (cons `(,(upat-uids (car upats))
				 ,@(gen-pattern-&-augment-unparser
				    (upat-pattern (car upats))))
			       branches)))))))))

		     


(defun gen-pattern-&-augment-unparser (pat)
  (assert (pattern-p pat))
  (let ((body 
	 (append
	  (cond ((get-pattern-augment pat)
		 (gen-augment-unparser (get-pattern-augment pat))))
	  (gen-pattern-unparser pat))))
    (if (pattern-gets-uterm? pat)
	`((unp-uterm ,(var (cond ((and (memq (pattern-kind pat)
					     '(alt opt))
				       (not (memq pat
						  *top-level-alts*)))
				  ;; @hack, documented in inter-phase
				  (1+ (get-pattern-slot pat)))
				 (t 
				  (get-pattern-slot pat))))
		     ,body))
	body)))




(defun pattern-gets-uterm? (pat)
  (or (is-pat-iterator pat)
      (and (get-pattern-binding-aug pat)
	   (pattern-augment pat)		; non-defaulted augment.
	   (not (memq (augment-kind (pattern-augment pat))
		      '(name ext-name))))))




(defun gen-pattern-unparser (pat &key (format-done nil))
  (if (and (not format-done)
	   (not (eq (pattern-kind pat) 'seq))
	   (is-sop 'seq pat)
	   (pattern-format pat))
         ;; In the above case the bp will not be written out be ordinary means
	 ;; so we have to fake it.  
      `((unp-seq (,(gen-pattern-unparser pat :format-done t))
		 (,(make-bp-spec (pattern-format pat)))))
      ;;  Indentation shift <----				       
  (ecase (pattern-kind pat)
    (nonterminal
     (cond ((is-lexical-terminal (pattern-leaf-ds pat)
				 *grammar-term*)
	    `((unp-lt ',(sbst-intern-case (pattern-leaf-ds pat))
		      ,(var (get-pattern-slot pat)))))
	   (t
	    `((unp-nt ,(mk-unparse-routine-name (pattern-leaf-ds pat))
		      ,(var (get-pattern-slot pat)))))))


    (ext-nonterminal
     (multiple-value-bind (nt lg-name)
	 (pattern-leaf-ds pat)
       `((unp-ext-nt
	  ,(intern (if *grammar-case-sensitive?*
		       (symbol-name 
			(mk-unparse-routine-name nt
						 :lg-name lg-name))
		       #+(and allegro (version>= 6))
		       (string-downcase
			(symbol-name 
			 (mk-unparse-routine-name nt
						  :lg-name lg-name)))
		       #-(and allegro (version>= 6))
		       (string-upcase
			(symbol-name 
			 (mk-unparse-routine-name nt
						  :lg-name lg-name))))
		   (lang:get-lang-code-package lg-name))
	  ,(intern (if *grammar-case-sensitive?*
		       (symbol-name nt)
		       #+(and allegro (version>= 6))
		       (string-dwoncase (symbol-name nt))
		       #-(and allegro (version>= 6))
		       (string-upcase (symbol-name nt)))
		   (lang:get-lang-abs-syn-package lg-name))
	  ,(var (get-pattern-slot pat))))))
    
    (ukeyword
     `((unp-keyword ',(sbst-intern-case (pattern-leaf-ds pat)))))

    (alt
     `((unp-alt ,(var (get-pattern-slot pat))
		,(mapcar #'gen-pattern-&-upattern-unparser
			 (pattern-sons pat)))))

    (opt
     `((unp-opt ,(var (get-pattern-slot pat))
		,(gen-pattern-&-upattern-unparser (car (pattern-sons pat))))))

    ((plus star)
     (let* ((indiv-slot
	     (cond ((memq (pattern-kind (pattern-son0 pat))
			  '(opt alt star doublestar))
		    (var (1+ (get-pattern-slot (pattern-son0 pat)))))
		   (t
		    (var (get-pattern-slot (pattern-son0 pat))))))
	    (body (gen-pattern-&-upattern-unparser (pattern-son0 pat)))
	    (uterm-body (if (pattern-gets-uterm? (pattern-son0 pat))
			    body
			    `((unp-uterm ,indiv-slot ,body)))))
     `((unp-rept
	,(var (get-pattern-slot pat))
	,uterm-body
	,indiv-slot
	,(make-bp-spec (pattern-format (pattern-son0 pat)))))))

    ((doubleplus doublestar)
     (let* ((indiv-slot
	     (cond ((memq (pattern-kind (pattern-son0 pat))
			  '(opt alt star doublestar))
		    (var (1+ (get-pattern-slot (pattern-son0 pat)))))
		   (t
		    (var (get-pattern-slot (pattern-son0 pat))))))
	    (body (gen-pattern-&-upattern-unparser (pattern-son0 pat)))
	    (uterm-body (if (pattern-gets-uterm? (pattern-son0 pat))
			    body
			    `((unp-uterm ,indiv-slot ,body)))))
       `((unp-double-rept 
	  ,(var (get-pattern-slot pat))
	  ,uterm-body
	  ,(gen-pattern-&-upattern-unparser (pattern-son1 pat))
	  ,indiv-slot
	  ,(make-bp-spec (pattern-format (pattern-son0 pat))
			 :kind :mid-double-rept)
	  ,(make-bp-spec
	    (pattern-format (pattern-son1 pat))
	    :kind :double-rept
	    :unite (or (and unite-double-rept
			    (not (eq 'sbst::|,|
				     (sbst-intern-ncase
				      (pattern-leaf-ds
				       (pattern-son1 pat))))))
		       (and unite-double-rept-commas
			    (eq 'sbst::|,|
				(sbst-intern-ncase
				 (pattern-leaf-ds
				  (pattern-son1 pat)))))))))))
    (seq 
     `((unp-seq
	,(mapcar #'gen-pattern-&-upattern-unparser
		 (pattern-sons pat))
	,(mapcar #'(lambda (x)
		     (make-bp-spec (pattern-format x)))
		 (pattern-sons pat)))))

    (jux
     `((unp-jux
	,(gen-pattern-&-upattern-unparser (car (pattern-sons pat)))
	',(mk-jux-name (pattern-name pat))
	,(gen-pattern-&-upattern-unparser (cadr (pattern-sons pat)))
	,(make-bp-spec (pattern-format (pattern-son0 pat)))
	,(make-bp-spec (pattern-format (pattern-son1 pat)))
	,(make-bp-spec (pattern-jux-ws pat))))))))
 





(defun gen-augment-unparser (aug)
  (ecase (augment-kind aug)

    (id `((unp-base-id)))
    (literal `((unp-base-lit)))
    (number `((unp-base-num)))
    (string `((unp-base-str)))

    (cons
     `((unp-cons)
       ,@(map-append #'gen-augment-unparser
		     (augment-args aug))))
    (bcons
     `((unp-bcons)
       ,@(map-append #'gen-augment-unparser
		     (augment-args aug))))
    (list
     `((unp-list)
       ,@(map-append #'gen-augment-unparser
		     (augment-args aug))))
    (append
     (unparse-gentime-error 
      (format nil "~% Warning: Append is not an unparsable construct: ~%")
      t))

    (term-const
     (cond ((has-elist-arg? aug)	; routines imported from sort-gen.
	    `((unp-elist-const)))
	   ((has-null-arg? aug)
	    `((unp-null-const)))
	   ((has-list-arg? aug)
	    `((unp-list-const)
	      ,@(map-append #'gen-augment-unparser
			    (cdr (augment-args aug)))))
					; CDR to get rid of op field.
	   (t
	    `((unp-term-const ',(augment-leaf-ds (augment-arg0 aug)))
	      ,@(map-append #'gen-augment-unparser
			    (cdr (augment-args aug)))))))
					; CDR to get rid of op field.
    
    ((name ext-name)
     (if (augment-key-slot aug)
	 `((unp-name ',(augment-leaf-ds aug)
		     ,(var (augment-key-slot aug))))
	 `((toss-name ',(augment-leaf-ds aug))))) ; for upatterns not used.
     
    
    ((bind internal-bind)
     `((unp-bind ',(augment-name aug)
		 ,(var (augment-result-slot aug)))
       ,@(gen-augment-unparser (car (augment-args aug)))))

    (opt
     (append
      (let ((discrim? (not (get-pattern-is-discriminated
			    (car (augment-path aug))))))
	   ;; This tells us whether the current alternation has already been
	   ;; discriminated by prevsiously encountered abstract syntax.
	(cond (discrim?
	       (set-pattern-is-discriminated (car (augment-path aug)) t)
	       `((dis-opt
		  ,(var (augment-key-slot aug))
		  (lambda (x)
		    ,(choose-discriminator-aux (augment-arg0 aug)
					       'x))
		  (lambda (x)
		    ,(choose-discriminator-aux (augment-arg1 aug)
					       'x)))))))
      `((unp-opt-aug ,(var (augment-key-slot aug))
		     ,(gen-augment-unparser (augment-arg0 aug))
		     ,(gen-augment-unparser (augment-arg1 aug))))))


    (alt
     (append
      (let ((discrim? (not (get-pattern-is-discriminated
			    (car (augment-path aug))))))
	   ;; This tells us whether the current alternation has already been
	   ;; discriminated by prevsiously encountered abstract syntax.
	(cond (discrim?
	       (set-pattern-is-discriminated (car (augment-path aug)) t)
	       `((dis-alt
		  ,(var (augment-key-slot aug))
		  ,(mapcar #'(lambda (au)
			       `(lambda (x)
				  ,(choose-discriminator-aux au 'x)))
			   (augment-args aug)))))))
      `((unp-alt-aug ,(var (augment-key-slot aug))
		     ,(mapcar #'gen-augment-unparser
			      (augment-args aug))))))

    ((star plus)
     `((toss-rept)))

    ;; The above will only be used for upatterns.  Pattern/aug iterators are
    ;;   removed. 
    ;;(error
    ;;  "No list patterns should be in the abstract syntax at this point")

    (and
     (map-append (function gen-augment-unparser)
		 (reverse (augment-args aug))))))   ;opposite of parsing order

;
;    (flatten
;     
;     (let* ((constructor-name (car (augment-args (car (augment-args aug)))))
;	    (nt-name (car (augment-args (cadr (augment-args aug))))))
;	    (key-medial (search-medial aug nt-name *grammar-term*))
;       (declare (ignore nt-name))
;       `(DS-FLATTEN ,argument
;		    ,constructor-name
;	
;	    ,(get-destructor constructor-name *grammar-term*)
;		    :left
;		    ,(cond ((<= (get-nt-op-prec key-medial
;						nt-name
;						medial-left
;						*grammar-term*)
;				(get-nt-op-prec key-medial
;						nt-name
;						medial-right
;						*grammar-term*))
;			    :left)
;			   (t
;			    :right)))))


	   


(defun gen-nt-discriminator (nt)
  `(defun ,(mk-discrim-routine-name (nt-name nt))
	  (as)
	  
     (or (is-nt-opsig? ',(nt-name-abs-package nt) as)
	 (,(make-discriminator-ttype
	    (sort:sort-table-lookup (sort:mk-sort-ttype
				     (nt-name-abs-package nt))
				    *sort-table*))
	  as))))


(defun make-discriminator-ttype (ttype)
  (assert (ttypep ttype))
  (cond
   ((sort:is-id-ttype ttype)
    `(lambda (x)
	     (match-id ',(sort:ds-id-ttype ttype) x)))
   ((sort:is-num-ttype ttype)
    `(lambda (x)
	     (match-num ,(sort:ds-num-ttype ttype) x)))
   ((sort:is-str-ttype ttype)
    `(lambda (x)
	     (match-str ,(sort:ds-str-ttype ttype) x)))
   ((sort:is-lit-ttype ttype)
    `(lambda (x)
	     (match-lit ',(sort:ds-lit-ttype ttype) x)))
   
   ((sort:is-null-ttype ttype)
    (error "Internal Error (see maintainer) ~%~
            The null ttype is not discriminatable."))

   ((sort:is-op-ttype ttype)
    `(lambda (x) 
	     (dis-op ',(oper:ds-sim-op (sort:ds-op-ttype ttype))
		     x)))

   ((sort:is-sort-ttype ttype)
    (cond ((member (sort:ds-sort-ttype ttype)
		   (mapcar #'nt-name-abs-package
			   (grammar-nonterminal-definitions *grammar-term*))
		   :test #'eq)
	   (mk-discrim-routine-name (sort:ds-sort-ttype ttype)))
	  ((is-lexical-terminal (sb-intern-ncase (sort:ds-sort-ttype ttype))
				*grammar-term*)
	   `(lambda (x)
	      (dis-lt ',(sbst-intern-ncase (sort:ds-sort-ttype ttype))
		      x)))
	  ((sort:sort-table-lookup ttype)
	   (make-discriminator-ttype (sort:sort-table-lookup ttype)))
	  (t
	   (unparse-gentime-error
	    (format nil
		"The sort ~S is neither a nonterminal, nor a lexical terminal.~%~
		 It is undefined in the grammar."
	      (sort:ds-sort-ttype ttype))
	    t))))


			   
   ((is-union-ttype ttype)
    `(lambda (x)
       (or ,@(mapcar #'(lambda (y)
			 `(,(make-discriminator-ttype y) x))
		     (sort:ds-union-ttype ttype)))))

   (t
    (unparse-gentime-error
     (format nil
	 "Illegal abstract syntax specified by the ttype ~S~%~
          (There is an outside possibility that this is an internal error.)~%"
       ttype)
     t)
    ())))





(defvar *pattern-augment-discriminated* (make-hash-table :test #'eq))

(defun choose-discriminator-aux (aug argument)
  (clrhash *pattern-augment-discriminated*)
  (choose-discriminator aug argument))



(defun choose-discriminator (aug argument)
  (ecase (augment-kind aug)
    (literal
     `(match-id ',(augment-leaf-ds aug)
		,argument))
    (id
     `(match-id ',(augment-leaf-ds aug)
		,argument))
    (number
     `(match-num ,(augment-leaf-ds aug)
		 ,argument))
    (string
     `(match-str ,(augment-leaf-ds aug)
		 ,argument))

    (bind
     (choose-discriminator (augment-arg0 aug) argument))

    ((name ext-name)
     (cond ((pattern-p (car (augment-path aug)))
	    (choose-pattern-discriminator (car (augment-path aug))
					  argument))
	   (t
	    (choose-discriminator (car (augment-path aug)) argument))))

    ((alt opt)
     `(or ,@(mapcar #'(lambda (au)
			(choose-discriminator au argument))
		    (augment-args aug))))

    ((list cons bcons append)
     (unparse-gentime-error
      (format nil
	  "Not a discriminatable augment:~%~S~%" aug)
      t))

    ((plus star and)
     (error "Illegal augment for choose-discriminator:" aug))

    (term-const 
     (cond ((has-elist-arg? aug)	; routines imported from sort-gen.
	    `(and (dis-op ',(augment-term-const-op aug)
			  ,argument)
		  (null (get-term-args ,argument))))
	   (t
	    `(and (dis-op ',(augment-term-const-op aug)
			  ,argument)
		  ,@(let (result)
		      (do ((sons (cdr (augment-args aug)) (cdr sons))
			   (i 0 (1+ i)))
			  ((null sons) (reverse result))
			(case (augment-kind (car sons))
			  ((term-const literal string number id)
			   (push (choose-discriminator
				  (car sons)
				  `(term-argn ,argument ,i))
				 result)))))))))))


(defun choose-pattern-discriminator (pat argument)
  (cond ((and (get-pattern-binding-aug pat)
	      (not (gethash pat *pattern-augment-discriminated*)))
	 (let (result)
	   (setf (gethash pat *pattern-augment-discriminated*) t)
					; The above hack eliminates infinite
					; recursion from cirularity in the
					; grammars (An augment may bind to and
					; reference the pattern it is
					; associated with.).
	   (setq result (choose-discriminator (get-pattern-binding-aug pat)
					    argument))
	   (setf (gethash pat *pattern-augment-discriminated*) nil)
					; (see comment above).  Here we want to
					; reset this so that later ferences to
					; the same pattern will follow the same
					; path.
	   result))
	(t
	 (case (pattern-kind pat)
	   ((ext-nonterminal nonterminal)
	    (let ((ttype (get-pattern-ttype pat)))
	      (cond ((member (sort:ds-sort-ttype ttype)
			     (mapcar #'nt-name-abs-package
				     (grammar-nonterminal-definitions
				      *grammar-term*))
			     :test #'eq)
		     `(,(mk-discrim-routine-name (sort:ds-sort-ttype ttype))
		       ,argument))
		    ((is-lexical-terminal (sb-intern-ncase
					   (sort:ds-sort-ttype ttype))
					  *grammar-term*)
		     `(dis-lt ',(sbst-intern-case (sort:ds-sort-ttype ttype))
			      ,argument))
		    ((sort:sort-table-lookup ttype)
		     `(,(make-discriminator-ttype
			 (sort:sort-table-lookup ttype))
		       ,argument))
		    (t
		     (unparse-gentime-error
		      (format nil
			  "The sort ~S is neither a nonterminal, ~
		           nor a lexical terminal.~%~
		 	   It is undefined in the grammar."
			(sort:ds-sort-ttype ttype))
		      t)))))
	   (t
	    (unparse-gentime-error
	     (format nil "Pattern is not discriminatable:~%~S~%" pat)
	     t))))))
 			



;
;(defun gen-occurrences (aug &optional (curr-occ (sbrt::nil-occ)))
;  (ecase (augment-kind aug)
;    ((literal id number string
;     ()))
;    (bind
;     (gen-occurrences (augment-arg0 aug)))
;    ((name ext-name)
;     (cond ((pattern-p (car (augment-path aug)))
;	    (choose-pattern-discriminator (car (augment-path aug))
;					  argument))
;	   (t
;	    (choose-discriminator (car (augment-path aug)) argument))))
;    ((alt opt)
;     `(OR ,@(mapcar #'(lambda (au)
;			(choose-discriminator au argument))
;		    (augment-args aug))))
;    ((list cons bcons append)
;     (unparse-gentime-error
;      (format nil
;	  "Not a discriminatable augment:~%~S~%" aug)
;      t))
;    ((plus star and)
;     (error "Illegal augment for choose-discriminator:" aug))
;    (term-const 
;     (cond ((has-elist-arg? aug)	; routines imported from sort-gen.
;	    `(AND (DIS-OP ',(get-term-const-symbol aug)
;			  ,argument)
;		  (NULL (GET-TERM-ARGS ,argument))))
;	   (t
;	    `(AND (DIS-OP ',(get-term-const-symbol aug)
;			  ,argument)
;		  ,@(let (result)
;		      (do ((sons (cdr (augment-args aug)) (cdr sons))
;			   (i 0 (1+ i)))
;			  ((null sons) (reverse result))
;			(case (augment-kind (car sons))
;			  ((term-const literal string number id)
;			   (push (choose-discriminator
;				  (car sons)
;				  `(TERM-ARGN ,argument ,i))
;				 result)))))))))))
;

   

(defun make-bp-spec (ws-spec-term &key (kind nil) (unite nil))
  (let ((bp-value 0)
	(crs nil)
	(bp-united-flag nil)
	(spaces nil)
	(ws-tokens nil))
    (do ((ws-specs (if ws-spec-term
		       (term-args ws-spec-term)
		       nil)
		   (cdr ws-specs)))
	((null ws-specs))
      (ecase (sim-term-op (car ws-specs))
	(unite
	 (setf bp-united-flag (cond ((is-number (term-arg0 (car ws-specs)))
				     (ds-number (term-arg0 (car ws-specs))))
				    (t
				     (ds-id (term-arg0 (car ws-specs)))))))
	(sp
	 (setf spaces
	       (+ (if (null spaces) 0 spaces)
		  (ds-number (term-arg0 (car ws-specs))))))
	(cr
	 (setf bp-value most-negative-fixnum)
	 (setf crs 0)
	 (push `(make-token :kind :whitespace
			    :subkind :cr
			    :value 1)
	       ws-tokens))
	(incr-bp
	 (setf bp-value (+ bp-value
			   (* (ds-number (term-arg0 (car ws-specs)))
			      sbrt::bp-rel-constant))))
	(decr-bp
	 (setf bp-value (+ bp-value
			   (* (- (ds-number (term-arg0 (car ws-specs))))
			      sbrt::bp-rel-constant))))
	(push-indent 
	 (push `(make-token :kind :whitespace
			    :subkind :indent
			    :value ,(ds-number (term-arg0 (car ws-specs))))
	       ws-tokens))
	(pop-indent 
	 (push `unindent-token
	       ws-tokens))
	(push-tab-left
	 (push `tab-left-token
	       ws-tokens))
	(push-tab-right
	 (push `tab-right-token
	       ws-tokens))
	(pop-tab
	 (push `untab-token
	       ws-tokens))
	))
    `(make-bp :value ,(if (and (eq kind :mid-double-rept)
			       (null ws-spec-term))
			  (+ bp-value mid-double-rept-bp-factor)
					; rather not break here.
			  bp-value)
	      :united-flag ,(if bp-united-flag
				`(list ,(if (numberp bp-united-flag)
					    bp-united-flag
					    `',bp-united-flag))
				(if (and unite
					 (null ws-spec-term))
				    `(list ',(gensym))))
	      
	      :spaces ,spaces
	      :crs ,crs
	      :format (list ,@(nreverse ws-tokens)))))



(defun unparse-gentime-error (string error)
  (terpri) 
  (cond (error
	 (setq *unparser-gen-error* t)
	 (format t "Unparser generation time error while processing - ~A~%"
	   *nt-name*))
	(t
	 (format t "Unparser generation time warning while processing - ~A~%"
	   *nt-name*)))
  (format t string)
  (terpri))


