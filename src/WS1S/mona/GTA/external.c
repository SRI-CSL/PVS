/* external.c */

/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "gta.h"
#include "bdd_external.h"
#include "memuser.h"

extern BddNode *table;
extern bdd_manager *import_bddm;

int gtaExport(GTA *G, char *filename, char *vars[], 
	      int orders[], SSSet *statespaces)
{
  unsigned i, j, l, r;
  FILE *file;
  unsigned numvars = 0;
  Table **tables = (Table **) mem_alloc(sizeof(Table*)*guide.numSs);

  for (i = 0; i < guide.numSs; i++)
    tables[i] = tableInit();
  
  if ((file = fopen(filename, "w")) == 0) 
    return 0;

  /* remove all marks G->bddm */
  for (i = 0; i < guide.numSs; i++)
    bdd_prepare_apply1(G->ss[i].bddm); 
  
  /* build table of tuples (idx,lo,hi) */
  for (i = 0; i < guide.numSs; i++) 
    for (l = 0; l < G->ss[guide.muLeft[i]].size; l++) 
      for (r = 0; r < G->ss[guide.muRight[i]].size; r++) 
	export(G->ss[i].bddm, 
	       BDD_ROOT(G->ss[i].bddm, BEH(G->ss[i], l, r)), 
	       tables[i]);

  for (i = 0; i < guide.numSs; i++) {
    for (j = 0; j < tables[i]->noelems; j++) {
      if (tables[i]->elms[j].idx != -1) {
	tables[i]->elms[j].lo = bdd_mark(G->ss[i].bddm, 
					 tables[i]->elms[j].lo)-1;
	tables[i]->elms[j].hi = bdd_mark(G->ss[i].bddm, 
					 tables[i]->elms[j].hi)-1;
      }
    }
  }

  while (vars[numvars] != 0)
    numvars++;

  fprintf(file,
          "MONA GTA\n"
	  "number of variables: %u\n"
          "state spaces: %u\n"
	  "universes: %u\n"
          "state space sizes:",
	  numvars, guide.numSs, guide.numUnivs);
  for (i = 0; i < guide.numSs; i++)
    fprintf(file, " %u", G->ss[i].size);
  fprintf(file, "\nfinal:");
  for (i = 0; i < G->ss[0].size; i++)
    fprintf(file, " %d", G->final[i]);

  fprintf(file, "\nguide:\n");
  for (i = 0; i < guide.numSs; i++)
    fprintf(file, " %s %u %u\n", guide.ssName[i], guide.muLeft[i], guide.muRight[i]);
  fprintf(file, "universes:\n");
  for (i = 0; i < guide.numUnivs; i++) 
    fprintf(file, " %s %s\n", guide.univName[i], guide.univPos[i]);
  fprintf(file, "variable orders and state spaces:\n");
  for (i = 0; i < numvars; i++) {
    fprintf(file, " %s %u:", vars[i], orders[i]);
    for (j = 0; j < guide.numSs; j++)
      if (statespaces[i][j])
	fprintf(file, " %d", j);
    fprintf(file, "\n");
  }

  for (i = 0; i < guide.numSs; i++) {
    fprintf(file, 
	    "\nstate space %u:\n"
	    " initial state: %u\n"
	    " bdd nodes: %u"
	    "\n behaviour:\n ", 
	    i , G->ss[i].initial, tables[i]->noelems);
    for (l = 0; l < G->ss[guide.muLeft[i]].size; l++) {
      for (r = 0; r < G->ss[guide.muRight[i]].size; r++) 
        fprintf(file, 
		" %u", 
		bdd_mark(G->ss[i].bddm, 
			 BDD_ROOT(G->ss[i].bddm, 
				  BEH(G->ss[i], l, r)))-1);
      fprintf(file, "\n ");
    }
    fprintf(file, "bdd:\n");
    for (j = 0; j < tables[i]->noelems; j++)
      fprintf(file, "  %i %u %u\n",
	      tables[i]->elms[j].idx, 
	      tables[i]->elms[j].lo,
	      tables[i]->elms[j].hi);
    
  }
  fprintf(file, "\nend\n");
  
  for (i = 0; i < guide.numSs; i++)
    tableFree(tables[i]);
  fclose(file);
  free(tables);
  return 1;
}
  
GTA *gtaImport(char *filename, char ***vars, int **orders, 
	       SSSet **statespaces, int set_guide)
{
  FILE *file;
  GTA *G;
  BddNode **tables;
  unsigned bddNodes, i, l, r, t, j;
  int numvars;
  char buffer[1000], buffer2[1000];
  int ti;
  
  unsigned f_numSs, f_numUnivs;
  SsId *f_muLeft, *f_muRight;
  char **f_ssName, **f_univPos, **f_univName;
  
  /* read file */
  if ((file = fopen(filename, "r")) == 0)  
    return 0;

  G = (GTA *) mem_alloc(sizeof(GTA));
  if (fscanf(file,
	     "MONA GTA\n"
	     "number of variables: %u\n"
             "state spaces: %u\n"
	     "universes: %u\n"
             "state space sizes:", 
	     &numvars, &f_numSs, &f_numUnivs) != 3)
    return 0;

  G->ss = (StateSpace *) mem_alloc(sizeof(StateSpace)*f_numSs);
  for (i = 0; i < f_numSs; i++) 
    if (fscanf(file, " %u", &G->ss[i].size) != 1)   
      return 0;
  fscanf(file, "\nfinal:");
  G->final = (int *) mem_alloc(sizeof(int)*G->ss[0].size);
  for (i = 0; i < G->ss[0].size; i++)
    fscanf(file, " %d", &G->final[i]);
  fscanf(file, "\nguide:\n");
  f_ssName = (char **) mem_alloc(sizeof(char *)*f_numSs);
  f_muLeft = (SsId *) mem_alloc(sizeof(SsId)*f_numSs);
  f_muRight = (SsId *) mem_alloc(sizeof(SsId)*f_numSs);
  for (i = 0; i < f_numSs; i++) {
    if (fscanf(file, "%s %u %u\n", 
               buffer, &f_muLeft[i], &f_muRight[i]) != 3)   
      return 0;
    f_ssName[i] = (char *) mem_alloc(strlen(buffer)+1);
    strcpy(f_ssName[i], buffer);
  }
  fscanf(file, "\nuniverses:\n");
  f_univName = (char **) mem_alloc(sizeof(char *)*f_numUnivs);
  f_univPos = (char **) mem_alloc(sizeof(char *)*f_numUnivs);
  for (i = 0; i < f_numUnivs; i++) {
    if (fscanf(file, "%s %s\n", buffer, buffer2) != 2)   
      return 0;
    f_univName[i] = (char *) mem_alloc(strlen(buffer)+1);
    strcpy(f_univName[i], buffer);
    f_univPos[i] = (char *) mem_alloc(strlen(buffer2)+1);
    strcpy(f_univPos[i], buffer2);
  }

  fscanf(file, "\nvariable orders and state spaces:\n");
  if (vars) {
    *vars = (char **) mem_alloc(sizeof(char *)*(numvars + 1));
    (*vars)[numvars] = 0;
  }
  if (orders)
    *orders = (int *) mem_alloc(sizeof(int)*numvars);
  if (statespaces)
    *statespaces = (SSSet *) mem_alloc(sizeof(SSSet)*numvars); 
  for (i = 0; i < numvars; i++) {
    char *b;
    if (orders)
      fscanf(file, " %s %d:", buffer, &((*orders)[i]));
    else
      fscanf(file, " %s %d:", buffer, &ti);
    if (vars) {
      (*vars)[i] = (char *) mem_alloc(strlen(buffer)+1);
      strcpy((*vars)[i], buffer);
    }
    fgets(buffer, 1000, file);
    for (b = buffer, j = 0; *b; b++)
      if (isspace((int) *b) && isdigit((int) *(b+1)))
	j++;
    if (statespaces) {
      (*statespaces)[i] = mem_alloc(f_numSs);
      for (t = 0; t < f_numSs; t++)
	(*statespaces)[i][t] = 0;
    }
    for (b = buffer; j > 0; j--) {
      int a;
      sscanf(b, " %d%n", &t, &a);
      if (statespaces)
	(*statespaces)[i][t] = 1;
      b += a;
    }
  }

  tables = (BddNode **) mem_alloc(sizeof(BddNode *)*f_numSs);
  for (i = 0; i < f_numSs; i++) {
    unsigned le = G->ss[f_muLeft[i]].size;
    unsigned ri = G->ss[f_muRight[i]].size;
    if (fscanf(file, 
	       "\nstate space %u:\n"
	       "initial state: %u\n"
	       "bdd nodes: %u\n"
	       "behaviour:\n", 
	       &t, &G->ss[i].initial, &bddNodes)!= 3)   
      return 0;
    G->ss[i].behaviour = (bdd_ptr *) mem_alloc(sizeof(bdd_ptr)*ri*le);
    G->ss[i].ls = le;
    G->ss[i].rs = ri;
    for (l = 0; l < le; l++) 
      for (r = 0; r < ri; r++) {
	fscanf(file, "%u ", &BEH(G->ss[i], l, r));
      }
    fscanf(file, "\nbdd:\n");
    tables[i] = (BddNode *) mem_alloc(sizeof(BddNode)*bddNodes);
    
    for (j = 0; j < bddNodes; j++) {
      tables[i][j].p = -1;
      fscanf(file, "%i %u %u\n", 
	     &tables[i][j].idx,
	     &tables[i][j].lo,
	     &tables[i][j].hi);
    }
  }

  if (fgetc(file) != 'e' ||
      fgetc(file) != 'n' ||
      fgetc(file) != 'd')   
    return 0;
  fclose(file);

  /* fill G->bddm */
  for (i = 0; i < f_numSs; i++) {
    table = tables[i];
    G->ss[i].bddm = 
      bdd_new_manager(8 * G->ss[i].size, G->ss[i].size*4); /* ?? */
    import_bddm = G->ss[i].bddm;
    for (l = 0; l < G->ss[f_muLeft[i]].size; l++) 
      for (r = 0; r < G->ss[f_muRight[i]].size; r++) {
	PUSH_SEQUENTIAL_LIST(import_bddm->roots, unsigned, 
			     make_node(BEH(G->ss[i], l, r)));
	BEH(G->ss[i], l, r) = BDD_LAST_HANDLE(import_bddm);
      }
  }

  if (set_guide) {
    /* set guide */
    makeGuide(f_numSs, f_muLeft, f_muRight, f_ssName, 
	      f_numUnivs, f_univPos, f_univName);

    if (!checkDisjoint() || !checkAllCovered() || !checkAllUsed()) 
      return 0;
  }
  else {
    /* check guide */
    if (f_numSs != guide.numSs || f_numUnivs != guide.numUnivs)
      return 0;
    for (i = 0; i < f_numSs; i++)
      if (f_muLeft[i] != guide.muLeft[i] ||
	  f_muRight[i] != guide.muRight[i])
	return 0;
      else 
	free(f_ssName[i]);
    for (i = 0; i < f_numUnivs; i++)
      if (strcmp(f_univPos[i], guide.univPos[i]) != 0)
	return 0;
      else {
	free(f_univPos[i]);
	free(f_univName[i]);
      }
    free(f_muLeft);
    free(f_muRight);
    free(f_ssName);
    free(f_univPos);
    free(f_univName);
  }

  for (i = 0; i < f_numSs; i++)
    free(tables[i]);
  free(tables);
  return G;
}
