//
// st_dfa.cpp
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#include <iostream.h>

#include "st_dfa.h"
#include "printline.h"
#include "env.h"
#include "offsets.h"
#include "timer.h"

extern Offsets     offsets;
extern Environment environment;

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

  if (environment.time) {
    timer_restrict.start();
    if (environment.statistics)
      temp.start();
  }

  if (environment.statistics) {
    cout << "Restrict";
    p.printsource();
    cout << "\n";
  }

  dfaRestrict(a);
  num_restricts++;

  if (environment.time) {
    timer_restrict.stop();
    if (environment.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  if (environment.intermediate)
    dfaPrintVerbose(a);

  update_largest(a);
  return a;
}

DFA* 
st_dfa_negation(DFA *a, Pos &p)
{
  Timer temp;

  if (environment.time) {
    timer_negation.start();
    if (environment.statistics)
      temp.start();
  }

  if (environment.statistics) {
    cout << "Negation";
    p.printsource();
    cout <<"\n";
  }

  dfaNegation(a);
  num_negations++;

  if (environment.time) {
    timer_negation.stop();
    if (environment.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  if (environment.intermediate)
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

  if (environment.time) {
    timer_product.start();
    if (environment.statistics)
      temp.start();
  }

  if (environment.statistics) {
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

  DFA *result = dfaProduct(a1, a2, ff); 
  num_products++;

  if (environment.statistics)
    cout << "(" << result->ns << "," << bdd_size(result->bddm) << ")\n";

  if (environment.time) {
    timer_product.stop();
    if (environment.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  dfaFree(a1);
  dfaFree(a2);

  if (environment.intermediate)
    dfaPrintVerbose(result);

  update_largest(result);
  return result;
}

DFA* 
st_dfa_project(DFA *a, unsigned i, Pos &p, bool quotient) 
{
  Timer temp1, temp2;

  int a_ns = a->ns;
  
  if (environment.time) {
    timer_right_quotient.start();
    if (environment.statistics)
      temp1.start();
  }

  if (environment.statistics)
    cout << "Right-quotient\n";

  if (quotient) {
    dfaRightQuotient(a, offsets.off(i));
    num_right_quotients++;
  }

  if (environment.time) {
    timer_right_quotient.stop();
    if (environment.statistics) {
      temp1.stop();
      cout << "  Time: ";
      temp1.print();
    }
  }
 
  if (environment.time) {
    timer_project.start();
    if (environment.statistics)
      temp2.start();
  }

  if (environment.statistics) {
    cout << "Projecting offset " << offsets.off(i);
    p.printsource();
    cout << "\n  (" << a_ns << "," << bdd_size(a->bddm) << ") -> ";
    cout.flush();
  }

  DFA *result = dfaProject(a, offsets.off(i));
  num_projections++;

  if (environment.statistics)
    cout << "("  << result->ns << "," << bdd_size(result->bddm) << ")\n";

  if (environment.time) {
    timer_project.stop();
    if (environment.statistics) {
      temp2.stop();
      cout << "  Time: ";
      temp2.print();
    }
  }

  dfaFree(a);

  if (environment.intermediate)
    dfaPrintVerbose(result);

  update_largest(result);
  return result;
}

DFA* 
st_dfa_minimization(DFA *a)
{
  Timer temp;

  int a_ns = a->ns;
  int a_sz = bdd_size(a->bddm);
  
  if (environment.time) {
    timer_minimization.start();
    if (environment.statistics)
      temp.start();
  }

  if (environment.statistics) {
    cout << "  Minimizing (" << a_ns << "," << a_sz << ") -> ";
    cout.flush();
  }

  DFA *result = dfaMinimize(a);
  num_minimizations++;

  if (environment.statistics)
    cout << "("  << result->ns << "," << bdd_size(result->bddm) << ")\n";

  if (environment.time) {
    timer_minimization.stop();
    if (environment.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  dfaFree(a);

  if (environment.intermediate)
    dfaPrintVerbose(result);

  update_largest(result);
  return result;
}

DFA*
st_dfa_copy(DFA *a)
{
  Timer temp;

  if (environment.time) {
    timer_copy.start();
    if (environment.statistics)
      temp.start();
  }

  if (environment.statistics) 
    cout << "Copying (" << a->ns << "," << bdd_size(a->bddm) << ")\n";
    
  DFA *result = dfaCopy(a);
  num_copies++;

  if (environment.time) {
    timer_copy.stop();
    if (environment.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  if (environment.intermediate)
    dfaPrintVerbose(result);

  update_largest(result);
  return result;
}

void 
st_dfa_replace_indices(DFA *a, IdentList *newvars, IdentList *oldvars,
		       bool offnew, bool offold)
{
  if (newvars && newvars != oldvars) {
  
    int *indexmap = new int[offsets.maxOffset()];
    
    IdentList::iterator i, j;
    bool dif = false;

    for(i = newvars->begin(), j = oldvars->begin();
	i != newvars->end(); i++, j++) {
      int theold = offold ? offsets.off(*j) : *j;
      int thenew = offnew ? offsets.off(*i) : *i;
      indexmap[theold] = thenew;
      if (theold != thenew)
	dif = true;
    }

    if (dif) {
      Timer temp;

      if (environment.time) {
	timer_replace_indices.start();
	if (environment.statistics)
	  temp.start();
      }

      if (environment.statistics)
	cout << "Replacing indices\n";

      dfaReplaceIndices(a, indexmap);
      num_replaces++;

      if (environment.time) {
	timer_replace_indices.stop();
	if (environment.statistics) {
	  temp.stop();
	  cout << "  Time: ";
	  temp.print();
	}
      }
    }    

    delete[] indexmap;

    if (environment.intermediate)
      dfaPrintVerbose(a);
  }

  update_largest(a);
}

DFA* 
st_dfa_prefix(DFA *a, Pos &p)
{
  Timer temp;

  if (environment.time) {
    timer_prefix.start();
    if (environment.statistics)
      temp.start();
  }

  if (environment.statistics) {
    cout << "Prefix";
    p.printsource();
    cout <<"\n";
  }

  dfaPrefixClose(a);
  num_prefixes++;

  if (environment.time) {
    timer_prefix.stop();
    if (environment.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  if (environment.intermediate)
    dfaPrintVerbose(a);

  update_largest(a);
  return a;
}

DFA*
st_dfa_lastpos(DFA *dfa, unsigned i)
{
  DFA *t1, *t2;

  t1 = st_dfa_minimization(st_dfa_product(dfa, 
					  dfaLast(i),
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
