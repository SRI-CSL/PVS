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
#include "st_gta.h"
#include "printline.h"
#include "env.h"
#include "offsets.h"
#include "timer.h"
#include "code.h"
#include "symboltable.h"
#include "codetable.h"

extern Offsets offsets;
extern Options options;
extern SymbolTable symbolTable;
extern CodeTable *codeTable;

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
  int dfasize = 0, bddsize = 0;
  for (s = 0; s < guide.numSs; s++) {
    dfasize += g->ss[s].size;
    bddsize += bdd_size(g->ss[s].bddm);
  }
  cout << "(" << dfasize << "," << bddsize << ")";
}

GTA *
st_gta_restrict(GTA *g, Pos &p)
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

  gtaRestrict(g);
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
    gtaPrintVerbose(g);

  update_largest(g);
  return g;
}

GTA *
st_gta_negation(GTA *g, Pos &p)
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
    cout << "\n";
  }

  gtaNegation(g);
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
    gtaPrintVerbose(g);

  update_largest(g);
  return g;
}

GTA * 
st_gta_product(GTA *g1, GTA *g2, gtaProductType ff, Pos &p)
{
  Timer temp;

  if (options.time) {
    timer_product.start();
    if (options.statistics)
      temp.start();
  }

  if (options.statistics) {
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

  codeTable->begin();
  GTA *result = gtaProduct(g1, g2, ff);
  codeTable->done();
  num_products++;

  if (options.statistics) {
    print_stat(result);
    cout << "\n";
  }

  if (options.time) {
    timer_product.stop();
    if (options.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  gtaFree(g1);
  gtaFree(g2);

  update_largest(result);
  return result;
}

GTA * 
st_gta_project(GTA *g, Ident i, Pos &p, bool quotient) 
{
  Timer temp;

  if (options.time) {
    timer_project.start();
    if (options.statistics)
      temp.start();
  }
  
  if (options.statistics) {
    cout << "Right-quotient\n" << "Projecting #" << i;
    p.printsource();
    cout << "\n  ";
    print_stat(g);
    cout << " -> ";
    cout.flush();
  }
  
  codeTable->begin();
  GTA *result = gtaQuotientAndProject(g, offsets.off(i), quotient);
  codeTable->done();
  
  num_projections++;
  num_right_quotients++;

  if (options.statistics) {
    print_stat(result);
    cout << "\n";
  }
  
  if (options.time) {
    timer_project.stop();
    if (options.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  gtaFree(g);

  update_largest(result);
  return result;
}

GTA * 
st_gta_minimization(GTA *g)
{
  Timer temp;

  if (options.time) {
    timer_minimization.start();
    if (options.statistics)
      temp.start();
  }

  if (options.statistics) {
    cout << "  Minimizing ";
    print_stat(g);
    cout << " -> ";
    cout.flush();
  }

  codeTable->begin();
  GTA *result = gtaMinimize(g);
  codeTable->done();
  num_minimizations++;

  if (options.statistics) {
    print_stat(result);
    cout << "\n";
  }
  
  if (options.time) {
    timer_minimization.stop();
    if (options.statistics) {
      temp.stop();
      cout << "  Time: ";
      temp.print();
    }
  }

  gtaFree(g);

  if (options.intermediate)
    gtaPrintVerbose(result);

  update_largest(result);
  return result;
}

GTA *
st_gta_copy(GTA *g)
{
  Timer temp;

  if (options.time) {
    timer_copy.start();
    if (options.statistics)
      temp.start();
  }

  if (options.statistics) {
    cout << "Copying ";
    print_stat(g);
    cout << "\n";
  }
  
  GTA *result = gtaCopy(g);
  num_copies++;

  if (options.time) {
    timer_copy.stop();
    if (options.statistics) {
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
  if (newvars && oldvars && newvars != oldvars) {
    invariant(newvars->size() == oldvars->size());
  
    unsigned *indexmap = new unsigned[offsets.maxOffset()];
    
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

      gtaReplaceIndices(g, indexmap);
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
  }

  update_largest(g);
}

GTA *
st_gta_allpos(GTA *gta, Ident i)
{
  GTA *t1, *t2;
  IdentList *u = symbolTable.allRealUnivs();
  SSSet set = stateSpaces(u);
  delete u;
  t1 = st_gta_minimization(st_gta_product(gta,
					  gtaAllPos(offsets.off(i), set),
					  gtaAND,
					  dummyPos));
  t2 = st_gta_minimization(st_gta_project(t1,
					  i,
					  dummyPos, false));
  update_largest(t2);
  return t2;
}
