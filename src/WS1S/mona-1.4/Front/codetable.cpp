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

#include <unistd.h>
#include <iostream.h>
#include <signal.h>
#include "codetable.h"
#include "env.h"

extern "C" {
#include "../Mem/mem.h"
}

extern Options options;

VarCode
CodeTable::insert(Code *c)
{
  unsigned hash = c->hash() % CODE_TABLE_SIZE;
  
  for (Deque<Code*>::iterator i = table[hash].begin(); 
       i != table[hash].end(); i++) 
    if ((*i)->equiv(*c)) {
      stat_hits++;
      (*i)->refs++;
      VarCode vc = VarCode(c->vars.copy(), *i);
      c->forwarded.remove();
      delete c; // replace Code-node in VarCode with old node
      return vc;
    }

  table[hash].push_back(c);
  c->bucket = hash;

  // update statistics
  stat_misses++;
  nodes++;
  switch (c->kind) {
  case cProject:
    num_proj++;
    break;
  case cAnd:
  case cOr:
  case cImpl:
  case cBiimpl:
    num_prod++;
    break;
  default: 
    num_other++;
    break;
  }

  return VarCode(&c->vars, c);
}

bool
CodeTable::exists(Code &c)
{ 
  unsigned hash = c.hash() % CODE_TABLE_SIZE;
  
  for (Deque<Code*>::iterator i = table[hash].begin(); 
       i != table[hash].end(); i++)
    if ((*i)->equiv(c))
      return true;
  return false;
}

Code*
CodeTable::findEquiv(Code *c)
{
  unsigned hash = c->hash() % CODE_TABLE_SIZE;
  
  for (Deque<Code*>::iterator i = table[hash].begin(); 
       i != table[hash].end(); i++)
    if (*i != c && (*i)->equiv(*c))
      return *i;
  return NULL;
}

void
CodeTable::remove(Code *c)
{
  unsigned b = c->bucket;
  Deque<Code*>::iterator i;
  int j;
  for (i = table[b].begin(), j = 0; i != table[b].end(); i++, j++)
    if (*i == c) {
      if (i+1 == table[b].end())
	table[b].pop_back();
      else
	table[b].set(j, table[b].pop_back());

      // update statistics
      nodes--;
      switch (c->kind) {
      case cProject:
	num_proj--;
	break;
      case cAnd:
      case cOr:
      case cImpl:
      case cBiimpl:
	num_prod--;
	break;
      default: 
	num_other--;
	break;
      }

      return;
    }
  invariant(false);
}

void
CodeTable::print_statistics()
{
  cout << "DAG hits: " << stat_hits
    /* << ", misses: " << stat_misses */
       << ", nodes: " << nodes << "\n";
}

void
CodeTable::print_statistics2()
{
  cout << "DAG nodes: " << nodes << "\n";
}

void
CodeTable::print_reduction_statistics()
{
  cout << "Projections removed: " << red_proj 
       << " (of " << num_proj+red_proj << ")\n"
       << "Products removed: " << red_prod 
       << " (of " << num_prod+red_prod << ")\n"
       << "Other nodes removed: " << red_other
       << " (of " << num_other+red_other << ")\n"
       << "DAG nodes after reduction: " << nodes << "\n";
}

void
CodeTable::print_sizes()
{
  unsigned i, j;
  cout << "\nDAG hash table bucket sizes:\n";
  for (i = 0; i < CODE_TABLE_SIZE; i++) 
    if (table[i].size() > 0) {
      cout << i << ": ";
      for (j = 0; j < table[i].size(); j++)
	cout << "*";
      cout << "\n";
    }
}

extern CodeTable *codeTable;

void
signal_handler(int sig)
{
  codeTable->update();
  signal(SIGALRM, signal_handler);
  alarm(1);
}

void 
CodeTable::init_print_progress()
{
  total_nodes = nodes;
  makes = 0;
  if (options.printProgress)
    signal(SIGALRM, signal_handler);
}

void 
CodeTable::begin()
{
  if (options.printProgress)
    alarm(1);
}

void 
CodeTable::done()
{
  if (options.printProgress)
    alarm(0);
}

int in_update = 0;

void 
CodeTable::update()
{
  if (in_update++) {
    in_update--;
    return;
  }
  int part = 100 * makes / total_nodes;
  long mem = (mem_allocated()+1048575)/1048576;
  if (options.statistics || options.intermediate) 
    cout << part << "% completed (" 
	 << dfa_in_mem+gta_in_mem << " automata in memory, " 
	 << mem << "MB memory used)\n";
  else {
    cout << "\r                                                                            ";
    cout << "\r" << part << "% completed";
    if (part == 100)
      cout << "                                                                         \n" 
	   << "Space: " << mem  << "MB\n";
    else 
      cout << " (" << dfa_in_mem+gta_in_mem << " automata in memory, " 
	   << mem  << "MB memory used)";
  }
  flush(cout);
  if (part<100)
    in_update--;
}

void 
CodeTable::print_progress()
{
  if (options.printProgress) {
    invariant(total_nodes > 0);
    makes++;
    int part = 100 * makes / total_nodes;
    if (part == 100 || !options.demo)
      if (part > prev) 
	update();
    prev = part;
  }
}

void
CodeTable::addSC(SubstCopy sc)
{
  sclist.push_back(sc);
}

void
CodeTable::clearSCTable()
{
  Deque<SubstCopy>::iterator s;
  for (s = sclist.begin(); s != sclist.end(); s++) {
    delete (*s).actuals;
    delete (*s).sign;
    (*s).vc.remove();
  }
  sclist.reset();
}


