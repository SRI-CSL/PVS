//
// st_dfa.h  
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#ifndef __ST_DFA_H
#define __ST_DFA_H

extern "C" {
#include "../DFA/dfa.h"
}

#include "ident.h"
#include "printline.h"

DFA *st_dfa_restrict(DFA *a, Pos &p);
DFA *st_dfa_negation(DFA *a, Pos &p);
DFA *st_dfa_product(DFA *a1, DFA *a2, dfaProductType ff, Pos &p);
DFA *st_dfa_project(DFA *a, unsigned i, Pos &p, bool quotient = true);
DFA *st_dfa_minimization(DFA *a);
DFA *st_dfa_copy(DFA *a);
void st_dfa_replace_indices(DFA *a, IdentList *newvars, IdentList *oldvars,
			    bool offnew = true, bool offold = true);
DFA *st_dfa_prefix(DFA *a, Pos &p);
DFA *st_dfa_lastpos(DFA *dfa, unsigned i);

void print_timing();
void print_statistics();

#endif
