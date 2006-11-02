;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

(in-package :pvs)

;; DFAs are packaged into a structure in order to finalize objects of this type

(defstruct (dfa
	    (:predicate dfa?)
	    (:constructor make-dfa (address))
	    (:print-function
	     (lambda (p s k)
	       (declare (ignore k))
	       (format s "~a" (dfa-address p)))))
  address)

(defun dfa= (p1 p2)
  (assert (dfa? p1))
  (assert (dfa? p2))
  (= (dfa-address p1) (dfa-address p2)))

(defun address (p)
  (dfa-address p))


(defun dfa-free! (p)
  (assert (dfa? p))
  (mona-free! (dfa-address p)))

(defun mk-dfa (a)
  (let ((p (make-dfa a)))
    #+allegro
    (excl:schedule-finalization p #'dfa-free!)
    #+cmu
    (ext:finalize p #'(lambda () (dfa-free! p)))
    p))


;; Constants

(let ((dfa-true nil))
  (defun dfa-true ()
    (or dfa-true
	(setq dfa-true (mk-dfa (mona-true))))))

(let ((dfa-false nil))
  (defun dfa-false ()
    (or dfa-false
	(setq dfa-false (mk-dfa (mona-false))))))

(defun dfa-true? (p)
  (dfa= p (dfa-true)))

(defun dfa-false? (p)
  (dfa= p (dfa-false)))

;; Status

(defun dfa-status (p)
  (assert (dfa? p))
  (let ((status (mona-status (address p))))
    (cond ((eql status 1) :valid)
	  ((eql status -1) :inconsistent)
	  ((eql status 0) :satisfiablebutnotvalid)
	  (t (error "Unreachable")))))

;; Variable

(defun dfa-var0 (i)
  (mk-dfa (mona-boolvar i)))

(defun dfa-var1 (i)                     ; X(0)*1(X)*
  (mk-dfa (mona-first-order i)))

;; Simple Constructors

(defun dfa-const (n i)               ; p_i = n
  (mk-dfa (mona-const n i)))

(defun dfa-less (i j)                ; p_i < p_j
  (mk-dfa (mona-less i j)))

(defun dfa-greater (i j)                ; p_i > p_j
  (mk-dfa (mona-less j i)))

(defun dfa-lesseq (i j)              ; p_i <= p_j
  (mk-dfa (mona-lesseq i j)))

(defun dfa-greatereq (i j)              ; p_i >= p_j
  (mk-dfa (mona-lesseq j i)))

(defun dfa-plus1 (i j n)             ;  p_i = p_j + n
  (mk-dfa (mona-plus1 i j n)))

(defun dfa-minus1 (i j n)            ;  p_i = p_j - n
  (mk-dfa (mona-minus1 i j n))) 

(defun dfa-eq1 (i j)                 ; p_i = p_j
  (mk-dfa (mona-eq1 i j)))

(defun dfa-eq2 (i j)                 ; P_i = P_j
  (mk-dfa (mona-eq2 i j)))

(defun dfa-plus2 (i j)               ; P_i = P_j + 1
  (mk-dfa (mona-plus2 i j)))

(defun dfa-minus2 (i j)              ; P_i = P_j + 1
  (mk-dfa (mona-minus2 i j)))

(defun dfa-plusmodulo1 (i j k)       ;  p_i = p_j + 1 % p_k
  (mk-dfa (mona-plusmodulo1 i j k)))

(defun dfa-minusmodulo1 (i j k)      ;  p_i = p_j - 1 % p_k
  (mk-dfa (mona-plusmodulo1 i j k)))

(defun dfa-empty (i)                 ; P_i = empty
  (mk-dfa (mona-empty i)))

(defun dfa-in (i j)                  ; p_i in P_j  recognizes <X,X>(<0,X>+)<1,1>(<X,X>*)
  (mk-dfa (mona-in i j)))

(defun dfa-subset (i j)              ; P_i sub P_j
  (mk-dfa (mona-subset i j)))

(defun dfa-union (i j k)             ; P_i = P_j union P_k
  (mk-dfa (mona-union i j k)))

(defun dfa-intersection (i j k)      ; P_i = P_j inter P_k
  (mk-dfa (mona-intersection i j k)))

(defun dfa-difference (i j k)        ; P_i = P_j \ P_k
  (mk-dfa (mona-difference i j k)))

(defun dfa-max (i j)                 ;  p_i = max(P_j)
  (mk-dfa (mona-max i j)))

(defun dfa-min (i j)                 ;  p_i = min(P_j)
  (mk-dfa (mona-min i j)))

(defun dfa-boolvar (i)               ; 1(X*)
  (mk-dfa (mona-boolvar i)))

(defun dfa-presburger-const (i n)    ; P_i = pconst(n)
  (mk-dfa (mona-presburger-const i n)))


;; DFA operations

(defun dfa-negation (p)
  (assert (dfa? p))
  (let ((a (mona-copy (address p))))
    (mona-negation! a)
    (let ((q (mk-dfa (mona-minimize a))))
      (mona-free! a)
      q)))

(defun dfa-conjunction (p1 p2)
  (assert (dfa? p1))
  (assert (dfa? p2))
  (let* ((a (mona-conjunction (address p1) (address p2)))
	 (p (mk-dfa (mona-minimize a))))
    (mona-free! a)
    p))

(defun dfa-disjunction (p1 p2)
  (assert (dfa? p1))
  (assert (dfa? p2))
  (let* ((a (mona-disjunction (address p1) (address p2)))
	 (p (mk-dfa (mona-minimize a))))
    (mona-free! a)
    p))

(defun dfa-implication (p1 p2)
  (assert (dfa? p1))
  (assert (dfa? p2))
  (let* ((a (mona-implication (address p1) (address p2)))
	 (p (mk-dfa (mona-minimize a))))
    (mona-free! a)
    p))

(defun dfa-iff (p1 p2)
  (assert (dfa? p1))
  (assert (dfa? p2))
  (let* ((a (mona-iff (address p1) (address p2)))
	 (p (mk-dfa (mona-minimize a))))
    (mona-free! a)
    p))

(defun dfa-unrestrict (p)
  (assert (dfa? p))
  (let ((a (mona-copy (address p))))
    (mona-unrestrict! a)
    (mk-dfa a)))
    
(defun dfa-exists (i p)
  (assert (dfa? p))
  (let ((a (mona-copy (address p))))
    (mona-right-quotient! a i)
    (let ((b (mona-project a i)))
      (mona-free! a)
      (mk-dfa (mona-minimize b)))))


;; Derived Automaton Constructions


(defun dfa-xor (p1 p2)
  (dfa-negation (dfa-iff p1 p2)))

(defun dfa-ite (c p1 p2)
  (dfa-disjunction (dfa-conjunction c p1)
		   (dfa-conjunction (dfa-negation c) p2)))

(defun dfa-conjunction* (as)
  (labels ((loop* (l acc)
	     (if (null l)
		 acc
	       (loop* (cdr l) (dfa-conjunction acc (car l))))))
    (loop* as (dfa-true))))

(defun dfa-exists2 (i p)
  (dfa-exists i p))

(defun dfa-exists1 (i p)
  (dfa-exists i (dfa-conjunction (dfa-var1 i) p)))

(defun dfa-exists0 (i p)
  (dfa-exists i p))

(defun dfa-forall2 (i p)
  (dfa-negation (dfa-exists i (dfa-negation p))))

(defun dfa-forall1 (i p)
  (dfa-negation (dfa-exists i (dfa-conjunction
			       (dfa-var1 i)
			       (dfa-negation p)))))

(defun dfa-forall0 (i p)
  (dfa-negation (dfa-exists i (dfa-negation p))))

(defun dfa-exists* (levels is a)
  (cond ((and (null levels) (null is))
	 a)
	((and (consp levels) (consp is))
	 (dfa-exists* (cdr levels)
		      (cdr is)
		      (let ((level (car levels)))
			(cond ((= level 2)
			       (dfa-exists2 (car is) a))
			      ((= level 1)
			       (dfa-exists1 (car is) a))
			      ((= level 0)
			       (dfa-exists0 (car is) a))
			      (t (error "Fatal Error: Unknown Level ~a" level))))))
	(t
	 (error "Fatal Error: List arguments not of equal length"))))

(defun dfa-forall* (levels is a)
  (dfa-negation (dfa-exists* levels is (dfa-negation a))))

(defun dfa-exists2* (is a)
  (if (null is) a
    (dfa-exists2 (car is) (dfa-exists2* (cdr is) a))))

(defun dfa-exists1* (is a)
  (if (null is) a
    (dfa-exists1 (car is) (dfa-exists1* (cdr is) a))))

(defun dfa-single (k i) ; P_k = {i}
  (mk-dfa
   (mona-minimize
    (mona-conjunction (mona-singleton k)
		      (mona-in i k)))))

;; Accessors, Recognizers

(defun number-of-states      (a) (mona-dfa-ns (address a)))
(defun start-state           (a) (mona-dfa-s (address a)))
(defun number-of-bdd-nodes   (a) (mona-bdd-size (dfa-bddm (address a))))
(defun number-of-transitions (a) (mona-transition-table-size (address a)))


;; Witnesses and Counterexamples

(defun dontcare? (ch) (eq ch #\X))
(defun set?      (ch) (eq ch #\1))
(defun unset?    (ch) (eq ch #\0))

(defun make-example (p num indices &key kind)
  (let ((char* (mona-make-example (address p) kind num
				  #+allegro indices
				  #+cmu (sys:vector-sap indices))))
    (if #+allegro (= 0 char*)
	#+cmu (or (not (stringp char*))
		  (string= char* ""))
	:null
	(let* ((str #+allegro (ff:char*-to-string char*)
		    #+cmu char*)
	       (len (/ #+allegro (ff:char*-string-length char*)
		       #+cmu (length char*)
		       (1+ num))))	; to do: free string
	  #+dbg (assert (integerp len))
	  (values str len)))))
		     
(defun dfa-witness (p num indices)
  (assert (dfa? p))
  (make-example p num indices :kind 1))
  
(defun dfa-counterexample (p num indices)
  (assert (dfa? p))
  (make-example p num indices :kind -1))


;; Printing a DFA

(defun dfa-print (p num fvars offsets)
  (mona-print (address p) num fvars offsets))

(defun dfa-print-verbose (p)
  (mona-print-verbose (address p)))











