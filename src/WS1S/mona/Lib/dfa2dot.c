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
