//
// st_gta.cpp
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#include <iostream.h>
#include "st_gta.h"
#include "printline.h"
#include "env.h"
#include "offsets.h"
#include "timer.h"
#include "code.h"
#include "symboltable.h"

extern Offsets     offsets;
extern Environment environment;
extern SymbolTable symbolTable;

extern Timer timer_restrict;
extern Timer timer_negation;
extern Timer timer_product;
extern Timer timer_right_quotient;
extern Timer timer_project;
extern Timer timer_minimization;
extern Timer timer_copy;
extern Timer timer_replace_indices;

extern unsigned num_minimizations;
extern unsigned num_projections;
extern unsigned num_products;
extern unsigned num_copies;
extern unsigned num_replaces;
extern unsigned num_right_quotients;
extern unsigned num_negations;
extern unsigned num_restricts;

extern int largest_states, largest_bdd;

void
update_largest(GTA *g)
{
  unsigned s;
  int states = 0, nodes = 0;
  for (s = 0; s < guide.numSs; s++) {
    states += g->ss[s].size;
    nodes += bdd_size(g->ss[s].bddm);
  }
  if (states > largest_states)
    largest_states = states;
  if (nodes > largest_bdd)
    largest_bdd = nodes;
}

void 
print_stat(GTA *g)
{
  unsigned s;
  cout << "(";
  for (s = 0; s < guide.numSs; s++) {
    cout << g->ss[s].size << "," << bdd_size(g->ss[s].bddm);
    if (s+1 < guide.numSs)
      cout << "; ";
  }
  cout << ")";
}

GTA *
st_gta_restrict(GTA *g, Pos &p)
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

  gtaRestrict(g);
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
    gtaPrintVerbose(g);

  update_largest(g);
  return g;
}

GTA *
st_gta_negation(GTA *g, Pos &p)
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
    cout << "\n";
  }

  gtaNegation(g);
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
    gtaPrintVerbose(g);

  update_largest(g);
  return g;
}

GTA * 
st_gta_product(GTA *g1, GTA *g2, gtaProductType ff, Pos &p)
{
  Timer temp;

  if (environment.time) {
    timer_product.start();
    if (environment.statistics)
      temp.start();
  }

  if (environment.statistics) {
    cout << "Product ";
    switch (ff) {
    case gtaAND:
      cout << "&";
      break;
    case gtaOR:
      cout << "|";
      break;
    case gtaIMPL:
      cout << "=>";
      break;
    case gtaBIIMPL:
      cout << "<=>";
      break;
    }
    p.printsource();
    cout << "\n  ";
    print_stat(g1);
    cout << "x";
    print_stat(g2);
    cout << " -> ";
    cout.flush();
  }

  GTA *result = gtaProduct(g1, g2, ff);
  num_products++;

  if (environment.statistics) {
    print_stat(result);
    cout << "\n";
  }

  if (environment.time) {
    timer_product.stop();
    if (environment.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  gtaFree(g1);
  gtaFree(g2);

  if (environment.intermediate)
    gtaPrintVerbose(result);
  
  update_largest(result);
  return result;
}

GTA * 
st_gta_project(GTA *g, unsigned i, Pos &p, bool quotient) 
{
  Timer temp;

  if (environment.time) {
    timer_project.start();
    if (environment.statistics)
      temp.start();
  }
  
  if (environment.statistics) {
    cout << "Right-quotient\n" << "Projecting offset " << offsets.off(i);
    p.printsource();
    cout << "\n  ";
    print_stat(g);
    cout << " -> ";
    cout.flush();
  }
  
  GTA *result = gtaQuotientAndProject(g, offsets.off(i), quotient);
  
  num_projections++;
  num_right_quotients++;

  if (environment.statistics) {
    print_stat(result);
    cout << "\n";
  }
  
  if (environment.time) {
    timer_project.stop();
    if (environment.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  gtaFree(g);

  if (environment.intermediate)
    gtaPrintVerbose(result);

  update_largest(result);
  return result;
}

GTA * 
st_gta_minimization(GTA *g)
{
  Timer temp;

  if (environment.time) {
    timer_minimization.start();
    if (environment.statistics)
      temp.start();
  }

  if (environment.statistics) {
    cout << "  Minimizing ";
    print_stat(g);
    cout << " -> ";
    cout.flush();
  }

  GTA *result = gtaMinimize(g);
  num_minimizations++;

  if (environment.statistics) {
    print_stat(result);
    cout << "\n";
  }
  
  if (environment.time) {
    timer_minimization.stop();
    if (environment.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  gtaFree(g);

  if (environment.intermediate)
    gtaPrintVerbose(result);

  update_largest(result);
  return result;
}

GTA *
st_gta_copy(GTA *g)
{
  Timer temp;

  if (environment.time) {
    timer_copy.start();
    if (environment.statistics)
      temp.start();
  }

  if (environment.statistics) {
    cout << "Copying ";
    print_stat(g);
    cout << "\n";
  }
  
  GTA *result = gtaCopy(g);
  num_copies++;

  if (environment.time) {
    timer_copy.stop();
    if (environment.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  update_largest(result);
  return result;
}

void
st_gta_replace_indices(GTA *g, IdentList *newvars, IdentList *oldvars,
		       bool offnew, bool offold)
{
  if (newvars && newvars != oldvars) {
  
    unsigned *indexmap = new unsigned[offsets.maxOffset()];
    
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

      gtaReplaceIndices(g, indexmap);
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
  }

  update_largest(g);
}

GTA *
st_gta_lastpos(GTA *gta, unsigned i)
{
  GTA *t1, *t2;
  SSSet set = stateSpaces(symbolTable.allRealUnivs());
  t1 = st_gta_minimization(st_gta_product(gta,
					  gtaLast(i, set),
					  gtaAND,
					  dummyPos));
  t2 = st_gta_minimization(st_gta_project(t1,
					  i,
					  dummyPos, false));
  update_largest(t2);
  return t2;
}
