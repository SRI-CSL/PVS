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

#include <iostream.h>

#include "st_dfa.h"
#include "printline.h"
#include "env.h"
#include "offsets.h"
#include "timer.h"
#include "codetable.h"

extern Offsets offsets;
extern Options options;
extern CodeTable *codeTable;

Timer timer_restrict;
Timer timer_negation;
Timer timer_product;
Timer timer_right_quotient;
Timer timer_project;
Timer timer_minimization;
Timer timer_copy;
Timer timer_replace_indices;
Timer timer_prefix;

unsigned num_minimizations = 0;
unsigned num_projections = 0;
unsigned num_products = 0;
unsigned num_copies = 0;
unsigned num_replaces = 0;
unsigned num_right_quotients = 0;
unsigned num_restricts = 0;
unsigned num_negations = 0;
unsigned num_prefixes = 0;

int largest_states = 0, largest_bdd = 0;

void
update_largest(DFA *a)
{
  if (a->ns > largest_states)
    largest_states = a->ns;
  if ((int) bdd_size(a->bddm) > largest_bdd)
    largest_bdd = bdd_size(a->bddm);
}

DFA* 
st_dfa_restrict(DFA *a, Pos &p)
{
  Timer temp;

  if (options.time) {
    timer_restrict.start();
    if (options.statistics)
      temp.start();
  }

  if (options.statistics) {
    cout << "Restrict";
    p.printsource();
    cout << "\n";
  }

  dfaRestrict(a);
  num_restricts++;

  if (options.time) {
    timer_restrict.stop();
    if (options.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  if (options.intermediate)
    dfaPrintVerbose(a);

  update_largest(a);
  return a;
}

DFA* 
st_dfa_negation(DFA *a, Pos &p)
{
  Timer temp;

  if (options.time) {
    timer_negation.start();
    if (options.statistics)
      temp.start();
  }

  if (options.statistics) {
    cout << "Negation";
    p.printsource();
    cout <<"\n";
  }

  dfaNegation(a);
  num_negations++;

  if (options.time) {
    timer_negation.stop();
    if (options.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  if (options.intermediate)
    dfaPrintVerbose(a);

  update_largest(a);
  return a;
}

DFA* 
st_dfa_product(DFA *a1, DFA *a2, dfaProductType ff, Pos &p)
{
  Timer temp;

  int a1_ns = a1->ns;
  int a2_ns = a2->ns;

  if (options.time) {
    timer_product.start();
    if (options.statistics)
      temp.start();
  }

  if (options.statistics) {
    cout << "Product ";
    switch (ff) {
    case dfaAND:
      cout << "&";
      break;
    case dfaOR:
      cout << "|";
      break;
    case dfaIMPL:
      cout << "=>";
      break;
    case dfaBIIMPL:
      cout << "<=>";
      break;
    }
    p.printsource();
    cout << "\n  (" << a1_ns << "," << bdd_size(a1->bddm) << ")x("
	 << a2_ns << "," << bdd_size(a2->bddm) << ") -> ";
    cout.flush();
  }

  codeTable->begin();
  DFA *result = dfaProduct(a1, a2, ff); 
  codeTable->done();
  num_products++;

  if (options.statistics)
    cout << "(" << result->ns << "," << bdd_size(result->bddm) << ")\n";

  if (options.time) {
    timer_product.stop();
    if (options.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  dfaFree(a1);
  dfaFree(a2);

  update_largest(result);
  return result;
}

DFA* 
st_dfa_project(DFA *a, Ident i, Pos &p, bool quotient) 
{
  Timer temp1, temp2;

  int a_ns = a->ns;
  
  if (options.time) {
    timer_right_quotient.start();
    if (options.statistics)
      temp1.start();
  }

  if (options.statistics)
    cout << "Right-quotient\n";

  if (quotient) {
    codeTable->begin();
    dfaRightQuotient(a, offsets.off(i));
    codeTable->done();
    num_right_quotients++;
  }

  if (options.time) {
    timer_right_quotient.stop();
    if (options.statistics) {
      temp1.stop();
      cout << "  Time: ";
      temp1.print();
    }
  }
 
  if (options.time) {
    timer_project.start();
    if (options.statistics)
      temp2.start();
  }

  if (options.statistics) {
    cout << "Projecting #" << i;
    p.printsource();
    cout << "\n  (" << a_ns << "," << bdd_size(a->bddm) << ") -> ";
    cout.flush();
  }

  codeTable->begin();
  DFA *result = dfaProject(a, offsets.off(i));
  codeTable->done();
  num_projections++;

  if (options.statistics)
    cout << "("  << result->ns << "," << bdd_size(result->bddm) << ")\n";

  if (options.time) {
    timer_project.stop();
    if (options.statistics) {
      temp2.stop();
      cout << "  Time: ";
      temp2.print();
    }
  }

  dfaFree(a);

  update_largest(result);
  return result;
}

DFA* 
st_dfa_minimization(DFA *a)
{
  Timer temp;

  int a_ns = a->ns;
  int a_sz = bdd_size(a->bddm);
  
  if (options.time) {
    timer_minimization.start();
    if (options.statistics)
      temp.start();
  }

  if (options.statistics) {
    cout << "  Minimizing (" << a_ns << "," << a_sz << ") -> ";
    cout.flush();
  }

  codeTable->begin();
  DFA *result = dfaMinimize(a);
  codeTable->done();
  num_minimizations++;

  if (options.statistics)
    cout << "("  << result->ns << "," << bdd_size(result->bddm) << ")\n";

  if (options.time) {
    timer_minimization.stop();
    if (options.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  dfaFree(a);

  if (options.intermediate)
    dfaPrintVerbose(result);

  update_largest(result);
  return result;
}

DFA*
st_dfa_copy(DFA *a)
{
  Timer temp;

  if (options.time) {
    timer_copy.start();
    if (options.statistics)
      temp.start();
  }

  if (options.statistics) 
    cout << "Copying (" << a->ns << "," << bdd_size(a->bddm) << ")\n";
    
  DFA *result = dfaCopy(a);
  num_copies++;

  if (options.time) {
    timer_copy.stop();
    if (options.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  if (options.intermediate)
    dfaPrintVerbose(result);

  update_largest(result);
  return result;
}

void 
st_dfa_replace_indices(DFA *a, IdentList *newvars, IdentList *oldvars,
		       bool offnew, bool offold)
{
  if (newvars && oldvars && newvars != oldvars) {
    invariant(newvars->size() == oldvars->size());
  
    int *indexmap = new int[offsets.maxOffset()];
    
    IdentList::iterator i, j;
    bool dif = false;

    for(i = newvars->begin(), j = oldvars->begin();
	j != oldvars->end(); i++, j++) {
      int theold = offold ? offsets.off(*j) : *j;
      int thenew = offnew ? offsets.off(*i) : *i;
      indexmap[theold] = thenew;
      if (theold != thenew)
	dif = true;
    }

    if (dif) {
      Timer temp;

      if (options.time) {
	timer_replace_indices.start();
	if (options.statistics)
	  temp.start();
      }

      if (options.statistics)
	cout << "Replacing indices\n";

      dfaReplaceIndices(a, indexmap);
      num_replaces++;

      if (options.time) {
	timer_replace_indices.stop();
	if (options.statistics) {
	  temp.stop();
	  cout << "  Time: ";
	  temp.print();
	}
      }
    }    

    delete[] indexmap;

    if (options.intermediate)
      dfaPrintVerbose(a);
  }

  update_largest(a);
}

DFA* 
st_dfa_prefix(DFA *a, Pos &p)
{
  Timer temp;

  if (options.time) {
    timer_prefix.start();
    if (options.statistics)
      temp.start();
  }

  if (options.statistics) {
    cout << "Prefix";
    p.printsource();
    cout <<"\n";
  }

  dfaPrefixClose(a);
  num_prefixes++;

  if (options.time) {
    timer_prefix.stop();
    if (options.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  if (options.intermediate)
    dfaPrintVerbose(a);

  update_largest(a);
  return a;
}

DFA*
st_dfa_lastpos(DFA *dfa, Ident i)
{
  DFA *t1, *t2;

  t1 = st_dfa_minimization(st_dfa_product(dfa, 
					  dfaLastPos(offsets.off(i)),
					  dfaAND,
					  dummyPos));
  t2 = st_dfa_minimization(st_dfa_project(t1, 
					  i,
					  dummyPos, false));
  update_largest(t2);
  return t2;
}

DFA*
st_dfa_allpos(DFA *dfa, Ident i)
{
  DFA *t1, *t2;

  t1 = st_dfa_minimization(st_dfa_product(dfa, 
					  dfaAllPos(offsets.off(i)),
					  dfaAND,
					  dummyPos));
  t2 = st_dfa_minimization(st_dfa_project(t1, 
					  i,
					  dummyPos, false));
  update_largest(t2);
  return t2;
}

void
print_timing()
{
  cout << "Minimize:       ";
  timer_minimization.print();

  cout << "Project:        ";
  timer_project.print();

  cout << "Product:        ";
  timer_product.print();

  cout << "Copy:           " ;
  timer_copy.print();

  cout << "Replace:        ";
  timer_replace_indices.print();

  cout << "Right-quotient: ";
  timer_right_quotient.print();

  cout << "Negate:         ";
  timer_negation.print();

  if (num_prefixes > 0) {
    cout << "Prefix:         ";
    timer_prefix.print();
  }
}

void
print_statistics()
{
  cout << "\nMinimizations:   " << num_minimizations
       << "\nProjections:     " << num_projections
       << "\nProducts:        " << num_products
       << "\nCopies:          " << num_copies
       << "\nReplaces:        " << num_replaces
       << "\nRight-quotients: " << num_right_quotients
       << "\nNegations:       " << num_negations << "\n";
  if (num_prefixes > 0) 
    cout << "Prefixes:        " << num_prefixes << "\n";

  cout << "\nLargest number of states: " << largest_states
       << ", BDD nodes: " << largest_bdd << "\n";
}
