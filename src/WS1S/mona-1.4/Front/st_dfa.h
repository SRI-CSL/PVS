/*
 * MONA
 * Copyright (C) 1997-2000 BRICS.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the  Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
 * USA.
 */

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
DFA *st_dfa_project(DFA *a, Ident i, Pos &p, bool quotient = true);
DFA *st_dfa_minimization(DFA *a);
DFA *st_dfa_copy(DFA *a);
void st_dfa_replace_indices(DFA *a, IdentList *newvars, IdentList *oldvars,
			    bool offnew = true, bool offold = true);
DFA *st_dfa_prefix(DFA *a, Pos &p);
DFA *st_dfa_lastpos(DFA *dfa, Ident i);
DFA *st_dfa_allpos(DFA *dfa, Ident i);

void print_timing();
void print_statistics();

#endif
