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

#ifndef __CODETABLE_H
#define __CODETABLE_H

#include "code.h"

#define CODE_TABLE_SIZE 1019

class CodeTable {
  Deque<Code*> table[CODE_TABLE_SIZE];
  Deque<SubstCopy> sclist; // used during reduction

public:
  CodeTable() 
  {stat_hits = stat_misses = nodes = makes = prev
     = red_proj = red_prod = red_other = num_prod = num_proj = num_other = 0;}

  VarCode insert(Code*); 
  bool    exists(Code&);
  Code   *findEquiv(Code*);
  void    remove(Code*);
  void    print_statistics();
  void    print_statistics2();
  void    print_reduction_statistics();
  void    print_sizes();
  void    init_print_progress();
  void    begin();
  void    update();
  void    done();
  void    print_progress();
  void    addSC(SubstCopy sc);
  void    clearSCTable();
  
  int stat_hits, stat_misses, nodes, total_nodes;

  int red_prod, red_proj, red_other; // number of reductions
  int num_prod, num_proj, num_other; // number of operations

  int makes, prev; // number of automata constructed
};

#endif
