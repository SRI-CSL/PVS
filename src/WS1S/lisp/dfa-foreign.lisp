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

(ff:def-c-type (mona-dfa :in-foreign-space) : struct
   (bddm * :unsigned)  ; Manager of BDD nodes  
   (ns     :int)       ; Number of states
   (q    * :unsigned)  ; Transition array
   (s      :int)       ; Start State   
   (f    * :int))      ; State Status Array

;; Predefined basic automata

(ff:def-foreign-call (mona-true "ws1s___dfaTrue")     ; true
    nil
  :returning :int)

(ff:def-foreign-call (mona-false "ws1s___dfaFalse")   ; false
     nil
  :returning :int)

(ff:def-foreign-call (mona-const "ws1s___dfaConst")  ; p_i = n
   ((n :int) (i :int))
  :returning :int)

(ff:def-foreign-call (mona-less "ws1s___dfaLess")   ; p_i < p_j
    ((i :int) (j :int))
  :returning :int)

(ff:def-foreign-call (mona-lesseq "ws1s___dfaLesseq")   ; p_i <= p_j
    ((i :int) (j :int))
  :returning :int)

(ff:def-foreign-call (mona-plus1 "ws1s___dfaPlus1")   ;  p_i = p_j + n
   ((i :int) (j :int) (n :int))
  :returning :int)

(ff:def-foreign-call (mona-minus1 "ws1s___dfaMinus1")   ;  p_i = p_j - n
   ((i :int) (j :int) (n :int))
  :returning :int)

(ff:def-foreign-call (mona-eq1 "ws1s___dfaEq1")    ; p_i = p_j
   ((i :int) (j :int))
  :returning :int)

(ff:def-foreign-call (mona-eq2 "ws1s___dfaEq2")      ; P_i = P_j
   ((i :int) (j :int))
  :returning :int)

(ff:def-foreign-call (mona-plus2 "ws1s___dfaPlus2")   ; P_i = P_j + 1
   ((i :int) (j :int))
  :returning :int)

(ff:def-foreign-call (mona-minus2 "ws1s___dfaMinus2")   ; P_i = P_j - 1
   ((i :int) (j :int))
  :returning :int)

(ff:def-foreign-call (mona-plusmodulo1 "ws1s___dfaPlusModulo1")   ;  p_i = p_j + 1 % p_k
   ((i :int) (j :int) (k :int))
  :returning :int)

(ff:def-foreign-call (mona-minusmodulo1 "ws1s___dfaMinusModulo1")   ;  p_i = p_j - 1 % p_k
   ((i :int) (j :int) (k :int))
  :returning :int)

(ff:def-foreign-call (mona-empty "ws1s___dfaEmpty")    ; P_i = empty
   ((i :int))
  :returning :int)

(ff:def-foreign-call (mona-in "ws1s___dfaIn") ; p_i in P_j  recognizes <X,X>(<0,X>+)<1,1>(<X,X>*)
    ((i :int) (j :int))
  :returning :int)

(ff:def-foreign-call (mona-subset "ws1s___dfaSubset")   ; P_i sub P_j
    ((i :int) (j :int))
  :returning :int)

(ff:def-foreign-call (mona-union "ws1s___dfaUnion")    ; P_i = P_j union P_k
   ((i :int) (j :int) (k :int))
  :returning :int)

(ff:def-foreign-call (mona-intersection "ws1s___dfaInter")    ; P_i = P_j inter P_k
   ((i :int) (j :int) (k :int))
  :returning :int)

(ff:def-foreign-call (mona-difference "ws1s___dfaSetminus")      ; P_i = P_j \ P_k
   ((i :int) (j :int) (k :int))
  :returning :int)

(ff:def-foreign-call (mona-max "ws1s___dfaMax")   ;  p_i = max(P_j)
   ((i :int) (j :int))
  :returning :int)

(ff:def-foreign-call (mona-min "ws1s___dfaMin")   ;  p_i = min(P_j)
   ((i :int) (j :int))
  :returning :int)


(ff:def-foreign-call (mona-boolvar "ws1s___dfaBoolvar")    ; 1(X*)
   ((i :int))
  :returning :int)

(ff:def-foreign-call (mona-presburger-const "ws1s___dfaPresbConst") ; P_i = pconst(n)
   ((i :int) (n :int))
  :returning :int)

(ff:def-foreign-call (mona-singleton "ws1s___dfaSingleton") ; (0*)1(0*)
   ((i :int))
  :returning :int)


(ff:def-foreign-call (mona-first-order "ws1s___dfaFirstOrder")       ; recognizes 0*1+
   ((i :int))
  :returning :int)


;; Automaton operations

(ff:def-foreign-call (mona-free! "ws1s___dfaFree")
    ((i :int))
  :returning :void)

(ff:def-foreign-call (mona-negation! "ws1s___dfaNegation")
    ((i :int))
  :returning :void)

(ff:def-foreign-call (mona-restrict! "ws1s___dfaRestrict")
    ((i :int))
  :returning :void)

(ff:def-foreign-call (mona-unrestrict! "ws1s___dfaUnrestrict")
    ((i :int))
  :returning :void)

(ff:def-foreign-call (mona-copy "ws1s___dfaCopy")
    ((i :int))
  :returning :int)

(ff:def-foreign-call (mona-product "ws1s___dfaProduct")
    ((a1 :int) (a2 :int) (mode :int)) ; (automaton * a1, a2, unsigned mode)
  :returning :int)

(ff:def-foreign-call (mona-prefix-close! "ws1s___dfaPrefixClose") ; Prefix Close
   ((i :int))
  :returning :void)

(ff:def-foreign-call (mona-conjunction "ws1s___dfaConjunction")
   ((a1 :int) (a2 :int)) ; (automaton * a1, a2)
  :returning :int)

(ff:def-foreign-call (mona-disjunction "ws1s___dfaDisjunction")
   ((a1 :int) (a2 :int)) ; (automaton * a1, a2)
  :returning :int)

(ff:def-foreign-call (mona-implication "ws1s___dfaImplication")
   ((a1 :int) (a2 :int)) ; (automaton * a1, a2)
  :returning :int)



(ff:def-foreign-call (mona-iff "ws1s___dfaIff")
   ((a1 :int) (a2 :int)) ; (automaton * a1, a2)
  :returning :int)

(ff:def-foreign-call (mona-status "ws1s___dfaStatus")
   ((a :int)) ; (automaton * a)
  :returning :int)

(ff:def-foreign-call (mona-project "ws1s___dfaProject")
					; projects away track var_index from a and                         
					; determinizes the resulting automaton
    ((a :int) (var_index :int)) ; (automaton * a, unsigned var_index)
  :returning :int)

(ff:def-foreign-call (mona-right-quotient! "ws1s___dfaRightQuotient")
    ((a :int) (var_index :int)) ; (automaton * a, unsigned var_index)
  :returning :void)

(ff:def-foreign-call (mona-minimize "ws1s___dfaMinimize") ; Minimization
    ((a :int))     ; automaton * a
  :returning :int)



;; Analysis and printing

(ff:def-foreign-call (mona-make-example "ws1s___dfaMakeExample")
    ((a :int)    ; DFA * a, 
     (kind :int) ; int kind
     (num :int)  ; int num
     (indices (:array :int))) ; unsigned indices()
  :returning :int)                ; char *
					

(ff:def-foreign-call (mona-analyze "ws1s___dfaAnalyze")
    ((a :int)				; DFA * a_impl
     (a_conj :int)			; DFA * a_conj
     (num :int)				; int num
     (names (:array (* :char))) ; char **names
     (orders (* :char))			; char * orders
     (treestyle :int))			; int treestyle
  :returning :void)

(ff:def-foreign-call (mona-print-vitals "ws1s___dfaPrintVitals")
    ((i :int))
  :returning :void)

(ff:def-foreign-call (mona-print "ws1s___dfaPrint")
    ((a :int)				; DFA * a
     (num :int)				; int num
     (names (:array (* :char))) ; char * names[]
     (indices (:array :fixnum)))	; unsigned indices()
  :returning :void)

(ff:def-foreign-call (mona-print-graphviz "ws1s___dfaPrintGraphviz")
    ((a :int)				; DFA * a
     (num :int)				; int num
     (indices (:array :fixnum)))	; unsigned indices()
  :returning :void)
					
(ff:def-foreign-call (mona-print-verbose "ws1s___dfaPrintVerbose")
    ((i :int))
  :returning :void)

(ff:def-foreign-call (bdd-size "ws1s___bdd_size")
    ((i :int))
  :returning :int)

;(ff:def-foreign-call 'transition-table-size
;   :entry-point "ws1s___transition_table_size"
;      '(integer)
;  :returning :int)

;; Constructing Automata Explicitly

(ff:def-foreign-call (mona-setup "ws1s___dfaSetup")
    ((n :int)				; int n
     (len :int)				; int len
     (indices (:array :fixnum)))	; int * indices
  :returning :void)

(ff:def-foreign-call (mona-alloc-exceptions "ws1s___dfaAllocExceptions")
    ((n :int))                ; int n
  :returning :void)

(ff:def-foreign-call (mona-store-exception "ws1s___dfaStoreException")
    ((s :int) (path (* :char)))        ; int s, char * path
  :returning :void)

(ff:def-foreign-call (mona-store-state "ws1s___dfaStoreState")
    ((s :int))        ; int s
  :returning :void)

(ff:def-foreign-call (mona-build "ws1s___dfaBuild")
    ((statuses (* :char)))  ; char * statuses
   :returning :int)

;; Exporting

(ff:def-foreign-call (mona-export "ws1s___dfaExport")
    ((a :int)				; DFA  *a
     (filename (* :char))			; char *filename
     (names (:array (* :char))) ; char *names()
     (orders (:array :fixnum)))	; int orders()
  :returning :int)
