;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diploma Thesis:
;;; "Solving Bit-Vector Equations 
;;;  - A Decision Procedure for Hardware Verifikation"
;;;
;;; M. Oliver M"oller
;;; University of Ulm
;;; Faculty of Computer Science (Informatik)
;;; AI Department (Abteilung f"ur k"unstliche Intelligenz)
;;; Supervising Professor:  Friedrich W. von Henke
;;;
;;; ------------------------------------------------------------
;;; -              SOLVER FOR BITVECTOR-THEORY                 -
;;; -              ~~~~~~~~~~~~~~~~~~~~~~~~~~~                 -
;;; - WITH  +fixed size                                        -
;;; -       +fixed extraction                                  -
;;; -       +composition                                       -
;;; -       +fills (constant- and non-constant)                -
;;; -       +boolean operations                                -
;;; ------------------------------------------------------------
;;; -      Oliver M"oller (moeller@ki.informatik.uni-ulm.de)   -
;;; ------------------------------------------------------------
;;; - File:     bvec_slicing.cl                                -
;;; - Purpose:  Provides a slicing over                        -
;;; -           flat processed bitvector terms                 -
;;; - USES:     bvec_structure.cl                              -
;;; -           bvec_bdd_solve.cl                              -
;;; ------------------------------------------------------------
;;;
;;; Allegro Common Lisp 
;;; Begun:         December, 15th 1997
;;; Last Changes:  March, 16th    1998
;;; Version 2.0
;;;
;;; NOTE: A slicing is a boolean vector with a 't at the top position;
;;;       it resembles the chunks a bit-vector is split into
;;;       eg. (aref slicing 0)=t ----> x[n] = .... o (x[n] ^(0,0)) 

(in-package bvec)

(defun bv-term-to-slicing (bv)
  (let* ((len (bv-length bv))
	 (res (make-nullslice len)))
    (if (rec-bv-composition? bv)
	(progn
	  (decf len)
	  (loop for e in (butlast (bv-composition-content bv)) do
		(decf len (bv-length e))
		(setf (aref res len) t))))
    res))


(defun is-nullslice? (sl) "Is a slicing empty t.i. unnecessary"
;checks, is sl is a vector that contains only nils and 
; [possibly] one 't' in the last position
       (and (arrayp sl)
	    (not (loop for i from 0 to (- (length sl) 2) do
		       (if (aref sl i) (return t))))))

(defun make-nullslice (n) "gives a nullslice of size n"
  (let ((s (make-array n)))
    (setf (aref s (- n 1)) t)
    s))

(defun make-fullslice (n) "gives a full-slice of size n"
  (make-array n :INITIAL-ELEMENT t))

(defun copy-slicing (old)
  (make-array (length old)
	      :initial-contents old))

(defun extract-slice (s left right)
  (EXCL::vector-subseq* s right (1+ left))) 

(defun slicing-to-chop-list (sl) "turn a slicing into a list of chopping positions, right to left"
  (let ((j 1)
	(res nil))
    (loop for i from 0 to (1- (length sl)) do
	  (if (aref sl i)
	      (progn (push j res)
		     (setf j 1))
	    (incf j)))
    (reverse res)))

(defun slicing-to-list (sl) "list of absolute positions"
  (loop for i from 0 to (1- (length sl)) append
	(if (aref sl i)
	    (list i)
	  nil)))
     

(defun list-to-slicing (l) "Turn a slicing to a list [top-pos = length]"
   (let ((res (make-array (1+ (apply #'max l)))))
     (loop for e in l do
	   (setf (aref res e) t))
     res))


(defun slice-bv-term (bv slicing &key (entry-slice-var nil))
  ;; Returns two-value results:
  ;;  (1) a list, ordered left to right
  ;; The (optional) function entry-slice-var is calles on
  ;;  possibly new chunks of variables, in order to update the
  ;;  current notion of slicing them
  (ifassert (eq (bv-length bv) (length slicing)))
  (let ((chl  (slicing-to-chop-list slicing))
	(args (reverse (bv-flat-term-content bv)))
	(actual-chl nil)
	(res nil)
	(pos 0)
	(len 0))
	;;(changes nil)
    (loop for a in args do
	  (setf len (bv-length a)
		actual-chl nil)
	  (loop while (< (apply #'+ actual-chl) len) do
		(setf actual-chl (nconc actual-chl (list (car chl)))
		      chl (cdr chl)))
	  (ifassert (= (apply #'+ actual-chl) len))
	  (setf pos 0)
	  (if (= 1 (length actual-chl))
	      (push a res)
	    (loop for c in actual-chl do
		  (let ((chunk (fixed-alpha (make-bv-extraction a (1- (+ c pos)) pos))))
		    (ifuncall entry-slice-var chunk)
		    (incf pos c)
		    (push chunk res)))))
    res))
		       

;;; ****************************************
;;; ** APPLICATION OF BOOLEAN OPERATIONS
;;; ****************************************

(defun or-of-slices! (s snewlist)
  ;; the first argument is changed destructively!
  ;; the list snewlist of slices is ordered such that
  ;;  the first element denotes the rightmost slice
  ;; returns nil
  (let ((n 0))
    (ifassert (= (length s) (apply #'+ (mapcar #'length snewlist)))) ;;!!;;
    (loop for e in snewlist do
	(loop for i from 0 to (1- (length e)) do
	      (if (aref e i)
		  (setf (aref s n) t))
	      (incf n)))))

(defun and-of-slices! (s snewlist)
  ;; the first argument is changed destructively!
  ;; the list snewlist of slices is ordered such that
  ;;  the first element denotes the rightmost slice
  ;; returns nil
  (let ((n 0))
    (ifassert (= (length s) (apply #'+ (mapcar #'length snewlist)))) ;;!!;;
    (loop for e in snewlist do
	(loop for i from 0 to (1- (length e)) do
	      (unless (aref e i)
		(setf (aref s n) nil))
	      (incf n))))
  (setf (aref s (1- (length s))) t))



;;; ------------------------------
;;;  Auxiliary Functions
;;; ------------------------------

(defun overlay-vector-list (l) "Performs an OR on a list of vectors"
  (let* ((n (length (car l)))
	 (a (make-array n)))
    (loop for i from 0 to (1- n) do
	  (loop for e in l do
		(if (aref e i)
		    (progn (setf (aref a i) t)
			   (return)))))
    a))

;;; For Nodes:

(defun nodes-match? (bdd1 bdd2)
  (if (leaf-node? bdd1)
      (or (and (true-node? bdd1)
	       (true-node? bdd2))
	  (and (false-node? bdd1)
	       (false-node? bdd2))))
  (let ((v1 (node-variable bdd1))
	(v2 (node-variable bdd2)))
    (and (rec-bv-extraction? v1)
	 (rec-bv-extraction? v2)
	 (equal (bv-extraction-bv v1) (bv-extraction-bv v2))
	 (eq (bv-extraction-right v1) (1+ (bv-extraction-left v2)))
	 (nodes-match? (node-then bdd1) (node-then bdd2))
	 (nodes-match? (node-else bdd1) (node-else bdd2)))))


(defun attach-nodes (bdd1 bdd2)
  (make-unique-node :VARIABLE (fixed-beta (make-bv-composition (node-variable bdd1) (node-variable bdd2)))
		    :ELSE (attach-nodes (node-else bdd1) (node-else bdd2))
		    :THEN (attach-nodes (node-then bdd1) (node-else bdd2))))


