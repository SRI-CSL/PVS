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

#include <stdlib.h>
#include <stdio.h>
#include "dfalib.h"

void mdDump(mdDfa *dfa, FILE *file)
{
  int i;

  fprintf(file, 
	  "digraph MONA_DFA_BDD {\n"
	  "  center = true;\n"
	  "  size = \"10.5,7.5\"\n"
	  "  orientation = landscape;\n"
	  "  node [shape=record];\n"
	  "   s1 [shape=record,label=\"");
  
  for (i = 0; i < dfa->states; i++) {
    fprintf(file, "{%d|<%d> %d}", 
	    (dfa->f[i] == mdREJECT) ? -1 :
	    (dfa->f[i] == mdACCEPT) ? 1 : 0,
	    i, i);
    if (i+1 < dfa->states)
      fprintf(file, "|");
  }
  fprintf(file, "\"];\n");
  
  fprintf(file, "  node [shape = circle];");
  for (i = 0; i < dfa->bddNodes; i++)
    if (dfa->bdd[i].idx != -1)
      fprintf(file, " %d [label=\"%d\"]", i, dfa->bdd[i].idx);
  fprintf(file, "\n  node [shape = box];");
  for (i = 0; i < dfa->bddNodes; i++)
    if (dfa->bdd[i].idx == -1)
      fprintf(file, " %d [label=\"%d\"]", i, dfa->bdd[i].lo);
  fprintf(file, "\n");
    
  for (i = 0; i < dfa->states; i++) 
    fprintf(file, " s1:%d -> %d [style=bold];\n", i, dfa->behaviour[i]);

  for (i = 0; i < dfa->bddNodes; i++) 
    if (dfa->bdd[i].idx != -1) {
      mNode lo = dfa->bdd[i].lo;
      mNode hi = dfa->bdd[i].hi;
	fprintf(file, " %d -> %d [style=dashed];\n", i, lo);
	fprintf(file, " %d -> %d [style=filled];\n", i, hi);
    }

  fprintf(file, "}\n");
}

int main(int argc, char *argv[])
{
  mdDfa *dfa;
  FILE *file;

  if (argc != 3) {
    printf("usage: dfa2dot <dfa-file> <dot-file>\n");
    exit(-1);
  }

  dfa = mdLoad(argv[1]);
  if (!dfa) {
    printf("dfa load error\n");
    exit(-1);
  }
  if (!(file = fopen(argv[2], "w"))) {
    printf("unable to write to %s\n", argv[2]);
    exit(-1);
  }
  mdDump(dfa, file);
  fclose(file);
  mdFree(dfa);

  exit(0);
}
