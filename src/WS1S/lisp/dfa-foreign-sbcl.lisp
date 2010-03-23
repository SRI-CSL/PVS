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

;; Structure of a DFA in foreign space
(sb-alien:define-alien-type nil
  (sb-alien:struct mona-dfa
   (bddm (* t))              ; Manager of BDD nodes
   (ns   (integer 32))       ; Number of states
   (q    (* t))              ; Transition array
   (s    (integer 32))       ; Start State
   (f    (* (integer 32))))) ; State Status Array

;; Predefined basic automata

(sb-alien:define-alien-routine ("ws1s___dfaTrue" mona-true)     ; true
  (* (sb-alien:struct mona-dfa)))

(sb-alien:define-alien-routine ("ws1s___dfaFalse" mona-false)   ; false
  (* (sb-alien:struct mona-dfa)))

(sb-alien:define-alien-routine ("ws1s___dfaConst" mona-const)   ; p_i = n
  (* (sb-alien:struct mona-dfa))
  (n (integer 32)) (i (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaLess" mona-less)     ; p_i < p_j
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)) (j (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaLesseq" mona-lesseq) ; p_i <= p_j
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)) (j (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaPlus1" mona-plus1)   ;  p_i = p_j + n
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)) (j (integer 32)) (n (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaMinus1" mona-minus1) ;  p_i = p_i - p_j
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)) (j (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaEq1" mona-eq1)       ; p_i = p_j
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)) (j (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaEq2" mona-eq2)       ; P_i = P_j
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)) (j (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaPlus2" mona-plus2)   ; P_i = P_j + 1
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)) (j (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaMinus2" mona-minus2) ; P_i = P_j - 1
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)) (j (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaPlusModulo1" mona-plusmodulo1) ;  p_i = p_j + 1 % p_k
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)) (j (integer 32)) (k (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaMinusModulo1" mona-minusmodulo1) ;  p_i = p_j - 1 % p_k
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)) (j (integer 32)) (k (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaEmpty" mona-empty)   ; P_i = empty
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaIn" mona-in) ; p_i in P_j  recognizes <X,X>(<0,X>+)<1,1>(<X,X>*)
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)) (j (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaSubset" mona-subset) ; P_i sub P_j
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)) (j (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaUnion" mona-union)   ; P_i = P_j union P_k
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)) (j (integer 32)) (k (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaInter" mona-intersection) ; P_i = P_j inter P_k
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)) (j (integer 32)) (k (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaSetminus" mona-difference) ; P_i = P_j \ P_k
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)) (j (integer 32)) (k (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaMax" mona-max)       ;  p_i = max(P_j)
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)) (j (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaMin" mona-min)       ;  p_i = min(P_j)
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)) (j (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaBoolvar" mona-boolvar) ; b_i
  (* (sb-alien:struct mona-dfa))
  (b (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaPresbConst" mona-presburger-const) ; P_i = pconst(n)
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)) (n (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaSingleton" mona-singleton) ; singleton(P_i)
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaFirstOrder" mona-first-order) ; recognizes 0*1+
  (* (sb-alien:struct mona-dfa))
  (i (integer 32)))


;; Automaton operations

(sb-alien:define-alien-routine ("ws1s___dfaFree" mona-free!)
  sb-alien:void
  (a (* (sb-alien:struct mona-dfa))))

(sb-alien:define-alien-routine ("ws1s___dfaNegation" mona-negation!)
  sb-alien:void
  (a (* (sb-alien:struct mona-dfa))))

(sb-alien:define-alien-routine ("ws1s___dfaRestrict" mona-restrict!)
  sb-alien:void
  (a (* (sb-alien:struct mona-dfa))))

(sb-alien:define-alien-routine ("ws1s___dfaUnrestrict" mona-unrestrict!)
  sb-alien:void
  (a (* (sb-alien:struct mona-dfa))))

(sb-alien:define-alien-routine ("ws1s___dfaCopy" mona-copy)
  (* (sb-alien:struct mona-dfa))
  (a (* (sb-alien:struct mona-dfa))))

(sb-alien:define-alien-routine ("ws1s___dfaProduct" mona-product)
  (* (sb-alien:struct mona-dfa))
  (a1 (* (sb-alien:struct mona-dfa)))
  (a2 (* (sb-alien:struct mona-dfa)))
  (mode (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaPrefixClose" mona-prefix-close!)
  sb-alien:void
  (a (* (sb-alien:struct mona-dfa))))

(sb-alien:define-alien-routine ("ws1s___dfaConjunction" mona-conjunction)
  (* (sb-alien:struct mona-dfa))
  (a1 (* (sb-alien:struct mona-dfa))) (a2 (* (sb-alien:struct mona-dfa))))

(sb-alien:define-alien-routine ("ws1s___dfaDisjunction" mona-disjunction)
  (* (sb-alien:struct mona-dfa))
  (a1 (* (sb-alien:struct mona-dfa))) (a2 (* (sb-alien:struct mona-dfa))))

(sb-alien:define-alien-routine ("ws1s___dfaImplication" mona-implication)
  (* (sb-alien:struct mona-dfa))
  (a1 (* (sb-alien:struct mona-dfa))) (a2 (* (sb-alien:struct mona-dfa))))

(sb-alien:define-alien-routine ("ws1s___dfaIff" mona-iff)
  (* (sb-alien:struct mona-dfa))
  (a1 (* (sb-alien:struct mona-dfa))) (a2 (* (sb-alien:struct mona-dfa))))

(sb-alien:define-alien-routine ("ws1s___dfaStatus" mona-status)
  (integer 32)
  (a (* (sb-alien:struct mona-dfa))))

(sb-alien:define-alien-routine ("ws1s___dfaProject" mona-project)
				; projects away track var_index from a and
				; determinizes the resulting automaton
  (* (sb-alien:struct mona-dfa))
  (a (* (sb-alien:struct mona-dfa))) (index (sb-alien:unsigned 32)))

(sb-alien:define-alien-routine ("ws1s___dfaRightQuotient" mona-right-quotient!)
  sb-alien:void
  (a (* (sb-alien:struct mona-dfa))) (index (sb-alien:unsigned 32)))

(sb-alien:define-alien-routine ("ws1s___dfaMinimize" mona-minimize) ; Minimization
  (* (sb-alien:struct mona-dfa))
  (a (* (sb-alien:struct mona-dfa))))


;; Analysis and printing

(sb-alien:define-alien-routine ("ws1s___dfaMakeExample" mona-make-example)
  sb-alien:c-string
  (a (* (sb-alien:struct mona-dfa)))
  (kind (integer 32))
  (num (integer 32))
  (indices (array (sb-alien:unsigned 32))))

(sb-alien:define-alien-routine ("ws1s___dfaAnalyze" mona-analyze)
  sb-alien:void
  (a (* (sb-alien:struct mona-dfa)))
  (num (integer 32))
  (names (array sb-alien:c-string))
  (indices (array sb-alien:unsigned))
  (orders (array sb-alien:char))
  (treestyle (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaPrintVitals" mona-print-vitals)
  sb-alien:void
  (a (* (sb-alien:struct mona-dfa))))

(sb-alien:define-alien-routine ("ws1s___dfaPrint" mona-print)
  sb-alien:void
  (a (* (sb-alien:struct mona-dfa)))
  (num (integer 32))
  (names (array sb-alien:c-string))
  (indices (array (sb-alien:unsigned 32))))

(sb-alien:define-alien-routine ("ws1s___dfaPrintGraphviz" mona-print-graphviz)
  sb-alien:void
  (a (* (sb-alien:struct mona-dfa)))
  (num (integer 32))
  (indices (array (sb-alien:unsigned 32))))

(sb-alien:define-alien-routine ("ws1s___dfaPrintVerbose" mona-print-verbose)
  sb-alien:void
  (a (* (sb-alien:struct mona-dfa))))

(sb-alien:define-alien-routine ("ws1s___bdd_size" bdd-size)
  (sb-alien:unsigned 32)
  (bbdm (* t)))


;; Constructing Automata Explicitly

(sb-alien:define-alien-routine ("ws1s___dfaSetup" mona-setup)
  sb-alien:void
  (s (integer 32))
  (len (integer 32))
  (indices (array (integer 32))))

(sb-alien:define-alien-routine ("ws1s___dfaAllocExceptions" mona-alloc-exceptions)
  sb-alien:void
  (n (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaStoreException" mona-store-exception)
  sb-alien:void
  (s (integer 32)) (path sb-alien:c-string))

(sb-alien:define-alien-routine ("ws1s___dfaStoreState" mona-store-state)
  sb-alien:void
  (s (integer 32)))

(sb-alien:define-alien-routine ("ws1s___dfaBuild" mona-build)
  (* (sb-alien:struct mona-dfa))
  (statuses (array sb-alien:char)))

;; Exporting

(sb-alien:define-alien-routine ("ws1s___dfaExport" mona-export)
  (integer 32)
  (a (* (sb-alien:struct mona-dfa)))
  (filename sb-alien:c-string)
  (num (integer 32))
  (names (array sb-alien:c-string))
  (orders (array sb-alien:char)))
