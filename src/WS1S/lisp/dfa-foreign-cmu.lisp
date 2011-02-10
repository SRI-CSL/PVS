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

;;(shadow 'INT :pvs)

;;(use-package :alien)
;;(use-package :c-call)

;; Structure of a DFA in foreign space
(alien:def-alien-type nil
  (alien:struct mona-dfa 
   (bddm (* alien:unsigned))  ; Manager of BDD nodes  
   (ns   alien:integer)       ; Number of states
   (q    (* alien:unsigned))  ; Transition array
   (s    alien:integer)       ; Start State   
   (f    (* alien:integer))))      ; State Status Array

;; Predefined basic automata

(alien:def-alien-routine ("ws1s___dfaTrue" mona-true)     ; true
  alien:integer)

(alien:def-alien-routine ("ws1s___dfaFalse" mona-false)   ; false
  alien:integer)

(alien:def-alien-routine ("ws1s___dfaConst" mona-const)  ; p_i = n
  alien:integer
  (n alien:integer) (i alien:integer))

(alien:def-alien-routine ("ws1s___dfaLess" mona-less)   ; p_i < p_j
  alien:integer
  (i alien:integer) (j alien:integer))

(alien:def-alien-routine ("ws1s___dfaLesseq" mona-lesseq)   ; p_i <= p_j
  alien:integer
  (i alien:integer) (j alien:integer))

(alien:def-alien-routine ("ws1s___dfaPlus1" mona-plus1)   ;  p_i = p_j + n
  alien:integer
  (i alien:integer) (j alien:integer) (n alien:integer))

(alien:def-alien-routine ("ws1s___dfaMinus1" mona-minus1)   ;  p_i = p_j - n
  alien:integer
  (i alien:integer) (j alien:integer) (n alien:integer))

(alien:def-alien-routine ("ws1s___dfaEq1" mona-eq1)    ; p_i = p_j
  alien:integer
  (i alien:integer) (j alien:integer))

(alien:def-alien-routine ("ws1s___dfaEq2" mona-eq2)      ; P_i = P_j
  alien:integer
  (i alien:integer) (j alien:integer))

(alien:def-alien-routine ("ws1s___dfaPlus2" mona-plus2)   ; P_i = P_j + 1
  alien:integer
  (i alien:integer) (j alien:integer))

(alien:def-alien-routine ("ws1s___dfaMinus2" mona-minus2)   ; P_i = P_j - 1
  alien:integer
  (i alien:integer) (j alien:integer))

(alien:def-alien-routine ("ws1s___dfaPlusModulo1" mona-plusmodulo1)   ;  p_i = p_j + 1 % p_k
  alien:integer
  (i alien:integer) (j alien:integer) (k alien:integer))

(alien:def-alien-routine ("ws1s___dfaMinusModulo1" mona-minusmodulo1)   ;  p_i = p_j - 1 % p_k
  alien:integer
  (i alien:integer) (j alien:integer) (k alien:integer))

(alien:def-alien-routine ("ws1s___dfaEmpty" mona-empty)    ; P_i = empty
  alien:integer
  (i alien:integer))

(alien:def-alien-routine ("ws1s___dfaIn" mona-in) ; p_i in P_j  recognizes <X,X>(<0,X>+)<1,1>(<X,X>*)
  alien:integer
  (i alien:integer) (j alien:integer))

(alien:def-alien-routine ("ws1s___dfaSubset" mona-subset)   ; P_i sub P_j
  alien:integer
  (i alien:integer) (j alien:integer))

(alien:def-alien-routine ("ws1s___dfaUnion" mona-union)    ; P_i = P_j union P_k
  alien:integer
  (i alien:integer) (j alien:integer) (k alien:integer))

(alien:def-alien-routine ("ws1s___dfaInter" mona-intersection)    ; P_i = P_j inter P_k
  alien:integer
  (i alien:integer) (j alien:integer) (k alien:integer))

(alien:def-alien-routine ("ws1s___dfaSetminus" mona-difference)      ; P_i = P_j \ P_k
  alien:integer
  (i alien:integer) (j alien:integer) (k alien:integer))

(alien:def-alien-routine ("ws1s___dfaMax" mona-max)   ;  p_i = max(P_j)
  alien:integer
  (i alien:integer) (j alien:integer))

(alien:def-alien-routine ("ws1s___dfaMin" mona-min)   ;  p_i = min(P_j)
  alien:integer
  (i alien:integer) (j alien:integer))


(alien:def-alien-routine ("ws1s___dfaBoolvar" mona-boolvar)    ; 1(X*)
  alien:integer
  (i alien:integer))

(alien:def-alien-routine ("ws1s___dfaPresbConst" mona-presburger-const) ; P_i = pconst(n)
  alien:integer
  (i alien:integer) (n alien:integer))

(alien:def-alien-routine ("ws1s___dfaSingleton" mona-singleton) ; (0*)1(0*)
  alien:integer
  (i alien:integer))


(alien:def-alien-routine ("ws1s___dfaFirstOrder" mona-first-order)       ; recognizes 0*1+
  alien:integer
  (i alien:integer))


;; Automaton operations

(alien:def-alien-routine ("ws1s___dfaFree" mona-free!)
  c-call:void
  (i alien:integer))

(alien:def-alien-routine ("ws1s___dfaNegation" mona-negation!)
  c-call:void
  (i alien:integer))

(alien:def-alien-routine ("ws1s___dfaRestrict" mona-restrict!)
  c-call:void
  (i alien:integer))

(alien:def-alien-routine ("ws1s___dfaUnrestrict" mona-unrestrict!)
  c-call:void
  (i alien:integer))

(alien:def-alien-routine ("ws1s___dfaCopy" mona-copy)
  alien:integer
  (i alien:integer))

(alien:def-alien-routine ("ws1s___dfaProduct" mona-product)
  alien:integer
  (a1 alien:integer) (a2 alien:integer) (mode alien:integer) ; (automaton * a1, a2, alien:unsigned mode)
  )

(alien:def-alien-routine ("ws1s___dfaPrefixClose" mona-prefix-close!) ; Prefix Close
  c-call:void
  (i alien:integer))

(alien:def-alien-routine ("ws1s___dfaConjunction" mona-conjunction)
  alien:integer
  (a1 alien:integer) (a2 alien:integer) ; (automaton * a1, a2)
  )

(alien:def-alien-routine ("ws1s___dfaDisjunction" mona-disjunction)
  alien:integer
  (a1 alien:integer) (a2 alien:integer) ; (automaton * a1, a2)
  )

(alien:def-alien-routine ("ws1s___dfaImplication" mona-implication)
  alien:integer
  (a1 alien:integer) (a2 alien:integer) ; (automaton * a1, a2)
  )



(alien:def-alien-routine ("ws1s___dfaIff" mona-iff)
  alien:integer
  (a1 alien:integer) (a2 alien:integer) ; (automaton * a1, a2)
  )

(alien:def-alien-routine ("ws1s___dfaStatus" mona-status)
  alien:integer
  (a alien:integer) ; (automaton * a)
  )

(alien:def-alien-routine ("ws1s___dfaProject" mona-project)
					; projects away track var_index from a and                         
					; determinizes the resulting automaton
  alien:integer
  (a alien:integer) (var_index alien:integer) ; (automaton * a, alien:unsigned var_index)
  )

(alien:def-alien-routine ("ws1s___dfaRightQuotient" mona-right-quotient!)
  c-call:void
  (a alien:integer) (var_index alien:integer) ; (automaton * a, alien:unsigned var_index)
  )

(alien:def-alien-routine ("ws1s___dfaMinimize" mona-minimize) ; Minimization
  alien:integer
  (a alien:integer)     ; automaton * a
  )



;; Analysis and printing

(alien:def-alien-routine ("ws1s___dfaMakeExample" mona-make-example)
  c-call:c-string
  (a alien:integer)    ; DFA * a, 
  (kind alien:integer) ; int kind
  (num alien:integer)  ; int num
  (indices (* alien:integer)) ; alien:unsigned indices()
  )                ; char *
					

(alien:def-alien-routine ("ws1s___dfaAnalyze" mona-analyze)
  c-call:void
  (a alien:integer)				; DFA * a_impl
  (a_conj alien:integer)			; DFA * a_conj
  (num alien:integer)				; int num
  (names c-call:c-string) ; char **names
  (orders c-call:c-string)			; char * orders
  (treestyle alien:integer)			; int treestyle
  )

(alien:def-alien-routine ("ws1s___dfaPrintVitals" mona-print-vitals)
  c-call:void
  (i alien:integer))

(alien:def-alien-routine ("ws1s___dfaPrint" mona-print)
  c-call:void
  (a alien:integer)				; DFA * a
  (num alien:integer)				; int num
  (names c-call:c-string) ; char * names[]
  (indices (array alien:unsigned))	; alien:unsigned indices()
  )

(alien:def-alien-routine ("ws1s___dfaPrintGraphviz" mona-print-graphviz)
  c-call:void
  (a alien:integer)				; DFA * a
  (num alien:integer)				; int num
  (indices (array alien:unsigned))	; alien:unsigned indices()
  )
					
(alien:def-alien-routine ("ws1s___dfaPrintVerbose" mona-print-verbose)
  c-call:void
  (i alien:integer))

(alien:def-alien-routine ("ws1s___bdd_size" bdd-size)
  alien:integer
  (i alien:integer))

;(alien:def-alien-routine 'transition-table-size
;   "ws1s___transition_table_size" :entry-point
;      '(alien:integer)
;  :returning int)

;; Constructing Automata Explicitly

(alien:def-alien-routine ("ws1s___dfaSetup" mona-setup)
  c-call:void
  (n alien:integer)				; int n
  (len alien:integer)				; int len
  (indices (array alien:unsigned))	; int * indices
  )

(alien:def-alien-routine ("ws1s___dfaAllocExceptions" mona-alloc-exceptions)
  c-call:void
  (n alien:integer)                ; int n
  )

(alien:def-alien-routine ("ws1s___dfaStoreException" mona-store-exception)
  c-call:void
  (s alien:integer) (path c-call:c-string)        ; int s, char * path
  )

(alien:def-alien-routine ("ws1s___dfaStoreState" mona-store-state)
  c-call:void
  (s alien:integer)        ; int s
  )

(alien:def-alien-routine ("ws1s___dfaBuild" mona-build)
  alien:integer
  (statuses c-call:c-string)  ; char * statuses
  )

;; Exporting

(alien:def-alien-routine ("ws1s___dfaExport" mona-export)
  alien:integer
  (a alien:integer)				; DFA  *a
  (filename c-call:c-string)			; char *filename
  (names (array c-call:c-string)) ; char *names()
  (orders (array alien:unsigned))	; int orders()
  )
