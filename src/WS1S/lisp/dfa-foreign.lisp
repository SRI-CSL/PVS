(in-package :pvs)

;; Structure of a DFA in foreign space

(ff:def-c-type (dfa :in-foreign-space) : struct
   (bddm * :unsigned)  ; Manager of BDD nodes  
   (ns     :int)       ; Number of states
   (q    * :unsigned)  ; Transition array
   (s      :int)       ; Start State   
   (f    * :int))      ; State Status Array

;; Predefined basic automata

(ff:defforeign 'dfa-true     ; true
  :entry-point "ws1s___dfaTrue"
  :arguments   nil
  :return-type :integer)

(ff:defforeign 'dfa-false    ; false
  :entry-point "ws1s___dfaFalse"
  :arguments   nil
  :return-type :integer)

(ff:defforeign 'dfa-const   ; p_i = n
  :entry-point "ws1s___dfaConst"
  :arguments '(integer integer) ; n i
  :return-type :integer)

(ff:defforeign 'dfa-less    ; p_i < p_j
  :entry-point "ws1s___dfaLess"
  :arguments '(integer integer) ; i j
  :return-type :integer)

(ff:defforeign 'dfa-lesseq    ; p_i <= p_j
  :entry-point "ws1s___dfaLesseq"
  :arguments '(integer integer) ; i j
  :return-type :integer)

(ff:defforeign 'dfa-plus1    ;  p_i = p_j + n
  :entry-point "ws1s___dfaPlus1"
  :arguments '(integer integer integer) ; i j n
  :return-type :integer)

(ff:defforeign 'dfa-minus1    ;  p_i = p_j - n
  :entry-point "ws1s___dfaMinus1"
  :arguments '(integer integer integer) ; i j n
  :return-type :integer)

(ff:defforeign 'dfa-eq1     ; p_i = p_j
  :entry-point "ws1s___dfaEq1"
  :arguments '(integer integer) ; i j
  :return-type :integer)

(ff:defforeign 'dfa-eq2       ; P_i = P_j
  :entry-point "ws1s___dfaEq2"
  :arguments '(integer integer);  i j
  :return-type :integer)

(ff:defforeign 'dfa-plus2    ; P_i = P_j + 1
  :entry-point "ws1s___dfaPlus2"
  :arguments '(integer integer)
  :return-type :integer)

(ff:defforeign 'dfa-minus2    ; P_i = P_j - 1
  :entry-point "ws1s___dfaMinus2"
  :arguments '(integer integer)
  :return-type :integer)

(ff:defforeign 'dfa-plusmodulo1    ;  p_i = p_j + 1 % p_k
  :entry-point "ws1s___dfaPlusModulo1"
  :arguments '(integer integer integer) ; i j k
  :return-type :integer)

(ff:defforeign 'dfa-minusmodulo1    ;  p_i = p_j - 1 % p_k
  :entry-point "ws1s___dfaMinusModulo1"
  :arguments '(integer integer integer) ; i j k
  :return-type :integer)

(ff:defforeign 'dfa-empty     ; P_i = empty
  :entry-point "ws1s___dfaEmpty"
  :arguments '(integer)
  :return-type :integer)

(ff:defforeign 'dfa-in         ; p_i in P_j  recognizes <X,X>(<0,X>+)<1,1>(<X,X>*)
  :entry-point "ws1s___dfaIn"
  :arguments '(integer integer) ; i j
  :return-type :integer)

(ff:defforeign 'dfa-subset    ; P_i sub P_j
  :entry-point "ws1s___dfaSubset"
  :arguments '(integer integer) ; i j
  :return-type :integer)

(ff:defforeign 'dfa-union     ; P_i = P_j union P_k
  :entry-point "ws1s___dfaUnion"
  :arguments '(integer integer integer) ; i j k
  :return-type :integer)

(ff:defforeign 'dfa-intersection     ; P_i = P_j inter P_k
  :entry-point "ws1s___dfaInter"
  :arguments '(integer integer integer) ; i j k
  :return-type :integer)

(ff:defforeign 'dfa-difference       ; P_i = P_j \ P_k
  :entry-point "ws1s___dfaSetminus"
  :arguments '(integer integer integer) ; i j k
  :return-type :integer)

(ff:defforeign 'dfa-max    ;  p_i = max(P_j)
  :entry-point "ws1s___dfaMax"
  :arguments '(integer integer) ; i j
  :return-type :integer)

(ff:defforeign 'dfa-min    ;  p_i = min(P_j)
  :entry-point "ws1s___dfaMin"
  :arguments '(integer integer) ; i j
  :return-type :integer)


(ff:defforeign 'dfa-boolvar     ; 1(X*)
  :entry-point "ws1s___dfaBoolvar"
  :arguments '(integer)
  :return-type :integer)

(ff:defforeign 'dfa-presburger-const ; P_i = pconst(n)
  :entry-point "ws1s___dfaPresbConst"
  :arguments '(integer integer)  ; i n
  :return-type :integer)

(ff:defforeign 'dfa-singleton ; (0*)1(0*)
  :entry-point "ws1s___dfaSingleton"
  :arguments '(integer)
  :return-type :integer)

(ff:defforeign 'dfa-last
  :entry-point "ws1s___dfaLast"
  :arguments '(integer)
  :return-type :integer)

(ff:defforeign 'dfa-first-order        ; recognizes 0*1+
  :entry-point "ws1s___dfaFirstOrder"
  :arguments '(integer)
  :return-type :integer)


;; Automaton operations

(ff:defforeign 'dfa-free!
  :entry-point "ws1s___dfaFree"
  :arguments   '(integer)
  :return-type :void)

(ff:defforeign 'dfa-negation!
  :entry-point "ws1s___dfaNegation"
  :arguments '(integer)
  :return-type :void)

(ff:defforeign 'dfa-restrict!
  :entry-point "ws1s___dfaRestrict"
  :arguments '(integer)
  :return-type :void)

(ff:defforeign 'dfa-unrestrict!
  :entry-point "ws1s___dfaUnrestrict"
  :arguments '(integer)
  :return-type :void)

(ff:defforeign 'dfa-copy
  :entry-point "ws1s___dfaCopy"
  :arguments   '(integer)
  :return-type :integer)

(ff:defforeign 'dfa-prefix-close! ; Prefix Close
  :entry-point "ws1s___dfaPrefixClose"
  :arguments '(integer)
  :return-type :void)

(ff:defforeign 'dfa-product
  :entry-point "ws1s___dfaProduct"
  :arguments '(integer integer integer) ; (automaton * a1, a2, unsigned mode)
  :return-type :integer)

(defconstant *ANDmode*   8)
(defconstant *ORmode*    14) 
(defconstant *IMPLmode*  11) 
(defconstant *BIMPLmode* 9)

(ff:defforeign 'dfa-project     ; projects away track var_index from a and                         
  :entry-point "ws1s___dfaProject"     ; determinizes the resulting automaton
  :arguments '(integer integer) ; (automaton * a, unsigned var_index)
  :return-type :integer)

(ff:defforeign 'dfa-right-quotient!
  :entry-point "ws1s___dfaRightQuotient"
  :arguments '(integer integer) ; (automaton * a, unsigned var_index)
  :return-type :void)

(ff:defforeign 'dfa-minimize           ; Minimization
  :entry-point "ws1s___dfaMinimize"
  :arguments '(integer)     ; automaton * a
  :return-type :integer)

;; Extensions

(ff:defforeign 'dfaFull
  :entry-point "ws1s___dfaIsFull"
  :arguments   '(integer)
  :return-type :integer)

(ff:defforeign 'dfaEmpty
  :entry-point "ws1s___dfaIsEmpty"
  :arguments   '(integer)
  :return-type :integer)

;; Analysis and printing

(ff:defforeign 'dfa-make-example
  :entry-point "ws1s___dfaMakeExample"
  :arguments   '(integer                ; DFA * a, 
		 integer                ; int kind
		 integer                ; int num
		 (simple-array fixnum)) ; unsigned indices()
  :return-type :integer)                ; char *
					
(ff:defforeign 'print-example
  :entry-point "ws1s___print_example"
  :arguments  '(integer                          ; char * example (here: use address)
	        string                           ; char * name
	        string                           ; char * description
	        integer                          ; int no_free_vars
	        (simple-array simple-string (*)) ; char **free_variables
	        (simple-array fixnum)            ; unsigned offsets()
	        string                           ; char * types
	        integer)                         ; int treestyle
  :return-type :void)

(ff:defforeign 'dfa-analyze
  :entry-point "ws1s___dfaAnalyze"
  :arguments   '(integer                          ; DFA * a_impl
		 integer                          ; DFA * a_conj
		 integer                          ; int num
		 (simple-array simple-string (*)) ; char **names
		 string                           ; char * orders
		 integer)                         ; int treestyle
  :return-type :void)

(ff:defforeign 'dfa-print-vitals
  :entry-point "ws1s___dfaPrintVitals"
  :arguments   '(integer)
  :return-type :void)

(ff:defforeign 'dfa-print
  :entry-point "ws1s___dfaPrint"
  :arguments   '(integer integer (simple-array simple-string (*)) (simple-array fixnum))
		; DFA * a, int num, char * names[], unsigned indices()
  :return-type :void)

(ff:defforeign 'dfa-print-graphviz
  :entry-point "ws1s___dfaPrintGraphviz"
  :arguments   '(integer integer (simple-array fixnum))  ; DFA * a, int num, unsigned indices()
  :return-type :void)
					
(ff:defforeign 'dfa-print-verbose
  :entry-point "ws1s___dfaPrintVerbose"
  :arguments   '(integer)
  :return-type :void)

(ff:defforeign 'bdd-size
   :entry-point "ws1s___bdd_size"
   :arguments   '(integer)
   :return-type :integer)

(ff:defforeign 'transition-table-size
   :entry-point "ws1s___transition_table_size"
   :arguments   '(integer)
   :return-type :integer)

;; Constructing Automata Explicitly

(ff:defforeign 'dfa-setup
  :entry-point "ws1s___dfaSetup"
  :arguments   '(integer                ; int n
		 integer                ; int len
		 (simple-array fixnum)) ; int * indices
  :return-type :void)

(ff:defforeign 'dfa-alloc-exceptions 
  :entry-point "ws1s___dfaAllocExceptions"
  :arguments   '(integer)                ; int n
  :return-type :void)

(ff:defforeign 'dfa-store-exception
  :entry-point "ws1s___dfaStoreException"
  :arguments   '(integer string)        ; int s, char * path
  :return-type :void)

(ff:defforeign 'dfa-store-state
  :entry-point "ws1s___dfaStoreState"
  :arguments   '(integer)        ; int s
  :return-type :void)

(ff:defforeign 'dfa-build
   :entry-point "ws1s___dfaBuild"
   :arguments   '(string)  ; char * statuses
   :return-type :integer)

;; Exporting

(ff:defforeign 'dfa-export
   :entry-point "ws1s___dfaExport"
   :arguments   '(integer                          ; DFA  *a
		  string                           ; char *filename
		  (simple-array simple-string (*)) ; char *names()
		  (simple-array fixnum))           ; int orders()
   :return-type :integer)
