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
#include "gtalib.h"

void mgDump(mgGta *gta, FILE *file)
{
  mgId d;

  fprintf(file, 
	  "digraph MONA_GTA_BDD {\n"
	  "  center = true;\n"
	  "  size = \"10.5,7.5\"\n"
	  "  orientation = landscape;\n\n");

  for (d = 0; d < gta->numSS; d++) {
    mNode n;
    mgId l,r;
    mgId d1 = gta->stateSpace[d].leftSS;
    mgId d2 = gta->stateSpace[d].rightSS; 
    fprintf(file,
	    "  node [shape = plaintext,\n"
	    "        label = \"state space: %s%s\\n\\n"
	    "left state space: %s\\nright state space: %s\\n"
	    "initial state: %u\"]; T_%u\n"
	    "  node [shape = record, label = \"", 
	    gta->stateSpace[d].name, 
	    (d == 0) ? " (root)" : "",
	    gta->stateSpace[d1].name, 
	    gta->stateSpace[d2].name,
	    gta->stateSpace[d].initial, d);
    for (l = 0; l < gta->stateSpace[d1].numStates; l++)
      for (r = 0; r < gta->stateSpace[d2].numStates; r++)
	fprintf(file, 
		"%s<F_%u_%u>(%u,%u)",
		(l > 0 || r > 0) ? "|" : "",
		l, r, l, r);
    fprintf(file, 
	    "\"]; B_%u\n"
	    "  T_%u -> B_%u [style = invis];\n"
	    "  node [shape = circle]; ", d, d, d);
    for (n = 0; n < gta->stateSpace[d].numBddNodes; n++)
      if (gta->stateSpace[d].bddNode[n].idx != -1)
	fprintf(file, " N_%u_%u [label = \"%u\"]", 
		d, n, gta->stateSpace[d].bddNode[n].idx);
    fprintf(file, "\n  node [shape = box];");
    for (n = 0; n < gta->stateSpace[d].numBddNodes; n++)
      if (gta->stateSpace[d].bddNode[n].idx == -1) {
	fprintf(file, " N_%u_%u [label = \"%u", 
		d, n, gta->stateSpace[d].bddNode[n].lo);
	if (d == 0)
	  fprintf(file, " (%d)", gta->final[gta->stateSpace[d].bddNode[n].lo]);
	fprintf(file, "\"]");
      }
    fprintf(file, "\n");
    for (l = 0; l < gta->stateSpace[d1].numStates; l++)
      for (r = 0; r < gta->stateSpace[d2].numStates; r++)
	fprintf(file, "  B_%u:F_%u_%u -> N_%u_%u [style = bold];\n",
	      d, l, r, d, gta->stateSpace[d].behaviour[l][r]);
    for (n = 0; n < gta->stateSpace[d].numBddNodes; n++) 
      if (gta->stateSpace[d].bddNode[n].idx != -1) {
	mNode lo = gta->stateSpace[d].bddNode[n].lo;
	mNode hi = gta->stateSpace[d].bddNode[n].hi;
	fprintf(file, " N_%u_%u -> N_%u_%u [style = dashed];\n", d, n, d, lo);
	fprintf(file, " N_%u_%u -> N_%u_%u [style = filled];\n", d, n, d, hi);
      }
  }
  fprintf(file, "}\n");
}

int main(int argc, char *argv[])
{
  mgGta *gta;
  FILE *file;

  if (argc != 3) {
    printf("usage: gta2dot <gta-file> <dot-file>\n");
    exit(-1);
  }

  gta = mgLoad(argv[1]);
  if (!gta) {
    printf("gta load error\n");
    exit(-1);
  }
  if (!(file = fopen(argv[2], "w"))) {
    printf("unable to write to %s\n", argv[2]);
    exit(-1);
  }
  mgDump(gta, file);
  fclose(file);
  mgFree(gta);

  exit(0);
}
