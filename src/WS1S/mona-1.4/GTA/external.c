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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "gta.h"
#include "bdd_external.h"
#include "../Mem/mem.h"

extern BddNode *table;
extern bdd_manager *import_bddm;

int gtaExport(GTA *G, char *filename, int num, char *vars[], 
	      char orders[], SSSet *statespaces, int opt_inhacc)
{
  unsigned i, j, k, l, r;
  FILE *file;
  boolean ***inherited = NULL;
  Table **tables = (Table **) mem_alloc(sizeof(Table*)*guide.numSs);
  if (opt_inhacc)
    inherited = gtaCalcInheritedAcceptance(G);

  for (i = 0; i < guide.numSs; i++)
    tables[i] = tableInit();
  
  if (filename) {
    if ((file = fopen(filename, "w")) == 0) 
      return 0;
  }
  else
    file = stdout;

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

  fprintf(file,
          "MONA GTA\n"
	  "number of variables: %u\n"
          "state spaces: %u\n"
	  "universes: %u\n"
          "state space sizes:",
	  num, guide.numSs, guide.numUnivs);
  for (i = 0; i < guide.numSs; i++)
    fprintf(file, " %u", G->ss[i].size);
  fprintf(file, "\nfinal:");
  for (i = 0; i < G->ss[0].size; i++)
    fprintf(file, " %d", G->final[i]);

  fprintf(file, "\nguide:\n");
  for (i = 0; i < guide.numSs; i++)
    fprintf(file, " %s %u %u\n", 
	    guide.ssName[i], guide.muLeft[i], guide.muRight[i]);
  fprintf(file, "types: %d\n", num_types);
  for (i = 0; i < num_types; i++) {
    fprintf(file, " type: %s\n", treetypes[i].name);
    fprintf(file, "  variants: %d\n", treetypes[i].numVariants);
    for (j = 0; j < treetypes[i].numVariants; j++) {
      fprintf(file, "  variant: %s %s\n", 
	      treetypes[i].variantName[j], 
	      treetypes[i].variantPos[j][0] ? 
	      treetypes[i].variantPos[j] : "-");
      fprintf(file, "  components: %d\n", treetypes[i].numComponents[j]);
      for (k = 0; k < treetypes[i].numComponents[j]; k++)
	fprintf(file, "   %s %s %d\n",
		treetypes[i].componentName[j][k],
		treetypes[i].componentPos[j][k][0] ? 
		treetypes[i].componentPos[j][k] : "-",
		treetypes[i].componentType[j][k]);
    }
  }
  fprintf(file, "universes:\n");
  for (i = 0; i < guide.numUnivs; i++) 
    fprintf(file, " %s %s\n", guide.univName[i], guide.univPos[i]);
  fprintf(file, "variable orders and state spaces:\n");
  for (i = 0; i < num; i++) {
    fprintf(file, " %s %u:", vars[i], (unsigned) orders[i]);
    for (j = 0; j < guide.numSs; j++)
      if (statespaces[i][j])
	fprintf(file, " %d", j);
    fprintf(file, "\n");
  }

  for (i = 0; i < guide.numSs; i++) {
    fprintf(file, 
	    "\nstate space %u:\n"
	    " initial state: %u\n"
	    " bdd nodes: %u",
	    i , G->ss[i].initial, tables[i]->noelems);
    if (inherited) {
      fprintf(file, "\n inherited-acceptance:");
      for (j = 0; j < G->ss[i].size; j++)
	fprintf(file, "\n  %u %u %u", 
		inherited[i][j][-1], inherited[i][j][0], inherited[i][j][1]);
    }
    fprintf(file, "\n behaviour:\n ");
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
  if (filename)
    fclose(file);
  mem_free(tables);
  if (opt_inhacc)
    gtaFreeInheritedAcceptance(inherited);
  return 1;
}
  
GTA *gtaImport(char *filename, char ***vars, int **orders, 
	       SSSet **statespaces, int set_guideandtypes)
{
  FILE *file;
  GTA *G;
  BddNode **tables;
  unsigned bddNodes, i, l, r, t, j, k;
  unsigned numvars;
  char buffer[1000], buffer2[1000];
  int ti;
  
  unsigned f_numSs, f_numUnivs;
  SsId *f_muLeft, *f_muRight;
  char **f_ssName, **f_univPos, **f_univName;
  int f_num_types;
  gtaType *f_treetypes;
  
  /* read file */
  if ((file = fopen(filename, "r")) == 0)  
    return 0;

  G = (GTA *) mem_alloc(sizeof(GTA));
  gta_in_mem++;
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

  if (fscanf(file, "\ntypes: %d\n", &f_num_types) != 1)
    return 0;
  f_treetypes = (gtaType *) mem_alloc(sizeof(gtaType)*f_num_types);
  for (i = 0; i < f_num_types; i++) {
    fscanf(file, "\ntype: %s\n", buffer);
    f_treetypes[i].name = (char *) mem_alloc(strlen(buffer)+1);
    strcpy(f_treetypes[i].name, buffer);
    if (fscanf(file, "\nvariants: %d\n", &f_treetypes[i].numVariants) != 1)
      return 0;
    f_treetypes[i].variantName = 
      (char **) mem_alloc(sizeof(char *)*f_treetypes[i].numVariants);
    f_treetypes[i].variantPos = 
      (char **) mem_alloc(sizeof(char *)*f_treetypes[i].numVariants);
    f_treetypes[i].numComponents = 
      (int *) mem_alloc(sizeof(int)*f_treetypes[i].numVariants);
    f_treetypes[i].componentName = 
      (char ***) mem_alloc(sizeof(char **)*f_treetypes[i].numVariants);
    f_treetypes[i].componentPos = 
      (char ***) mem_alloc(sizeof(char **)*f_treetypes[i].numVariants);
    f_treetypes[i].componentType = 
      (int **) mem_alloc(sizeof(int *)*f_treetypes[i].numVariants);
    for (j = 0; j < f_treetypes[i].numVariants; j++) {
      fscanf(file, "\nvariant: %s ", buffer);
      f_treetypes[i].variantName[j] = (char *) mem_alloc(strlen(buffer)+1);
      strcpy(f_treetypes[i].variantName[j], buffer);
      fscanf(file, "%s\n", buffer);
      f_treetypes[i].variantPos[j] = (char *) mem_alloc(strlen(buffer)+1);
      if (buffer[0]=='-')
	buffer[0] = 0;
      strcpy(f_treetypes[i].variantPos[j], buffer);
      if (fscanf(file, "\ncomponents: %d\n", &f_treetypes[i].numComponents[j]) != 1)
	return 0;
      f_treetypes[i].componentName[j] =
	(char **) mem_alloc(sizeof(char *)*f_treetypes[i].numComponents[j]);
      f_treetypes[i].componentPos[j] =
	(char **) mem_alloc(sizeof(char *)*f_treetypes[i].numComponents[j]);
      f_treetypes[i].componentType[j] =
	(int *) mem_alloc(sizeof(int)*f_treetypes[i].numComponents[j]);
      for (k = 0; k < f_treetypes[i].numComponents[j]; k++) {
	fscanf(file, "\n%s ", buffer);
	f_treetypes[i].componentName[j][k] = (char *) mem_alloc(strlen(buffer)+1);
	strcpy(f_treetypes[i].componentName[j][k], buffer);
	fscanf(file, "%s ", buffer);
	f_treetypes[i].componentPos[j][k] = (char *) mem_alloc(strlen(buffer)+1);
	if (buffer[0]=='-')
	  buffer[0] = 0;
	strcpy(f_treetypes[i].componentPos[j][k], buffer);
	fscanf(file, "%d\n", &f_treetypes[i].componentType[j][k]);
      }
    }
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
    if (orders)
      fscanf(file, " %s %d:", buffer, &((*orders)[i]));
    else
      fscanf(file, " %s %d:", buffer, &ti);
    if (vars) {
      (*vars)[i] = (char *) mem_alloc(strlen(buffer)+1);
      strcpy((*vars)[i], buffer);
    }
    if (statespaces) {
      (*statespaces)[i] = mem_alloc(f_numSs);
      for (t = 0; t < f_numSs; t++)
	(*statespaces)[i][t] = 0;
    }
    while (1) {
      fscanf(file, " %u", &t);
      if (statespaces)
	(*statespaces)[i][t] = 1;
      if (fgetc(file)=='\n')
	break;
    }
  }

  tables = (BddNode **) mem_alloc(sizeof(BddNode *)*f_numSs);
  for (i = 0; i < f_numSs; i++) {
    unsigned le = G->ss[f_muLeft[i]].size;
    unsigned ri = G->ss[f_muRight[i]].size;
    if (fscanf(file, 
	       "\nstate space %u:\n"
	       "initial state: %u\n"
	       "bdd nodes: %u\n",
	       &t, &G->ss[i].initial, &bddNodes) != 3)
      return 0;
    fscanf(file, "%s", buffer);
    if (strcmp(buffer, "inherited-acceptance:") == 0) {
      unsigned t;
      for (j = 0; j < G->ss[i].size; j++)
	fscanf(file, "\n%u %u %u ", &t, &t, &t);
      fscanf(file, "behaviour:\n");
    }
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

  if (set_guideandtypes) {
    /* set guide */
    makeGuide(f_numSs, f_muLeft, f_muRight, f_ssName, 
	      f_numUnivs, f_univPos, f_univName, 0, 0);

    if (!checkDisjoint() || !checkAllCovered() || !checkAllUsed()) 
      return 0;

    /* set types */
    num_types = f_num_types;
    treetypes = f_treetypes;
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
	mem_free(f_ssName[i]);
    for (i = 0; i < f_numUnivs; i++)
      if (strcmp(f_univPos[i], guide.univPos[i]) != 0)
	return 0;
      else {
	mem_free(f_univPos[i]);
	mem_free(f_univName[i]);
      }
    mem_free(f_muLeft);
    mem_free(f_muRight);
    mem_free(f_ssName);
    mem_free(f_univPos);
    mem_free(f_univName);

    /* check types */
    if (f_num_types != num_types)
      return 0;
    for (i = 0; i < num_types; i++) {
      if (f_treetypes[i].numVariants != treetypes[i].numVariants)
	return 0;
      else 
	for (j = 0; j < treetypes[i].numVariants; j++) {
	  if (strcmp(f_treetypes[i].variantPos[j],
		     treetypes[i].variantPos[j]) != 0 ||
	      f_treetypes[i].numComponents[j] != 
	      treetypes[i].numComponents[j])
	    return 0;
	  else
	    for (k = 0; k < treetypes[i].numComponents[j]; k++)
	      if (strcmp(f_treetypes[i].componentPos[j][k],
			 treetypes[i].componentPos[j][k]) != 0 ||
		  f_treetypes[i].componentType[j][k] !=
		  treetypes[i].componentType[j][k])
		return 0;
	      else {
		mem_free(f_treetypes[i].componentName[j][k]);
		mem_free(f_treetypes[i].componentPos[j][k]);
	      }
	  mem_free(f_treetypes[i].variantName[j]);
	  mem_free(f_treetypes[i].variantPos[j]);
	  mem_free(f_treetypes[i].componentName[j]);
	  mem_free(f_treetypes[i].componentPos[j]);
	  mem_free(f_treetypes[i].componentType[j]);
	}
      mem_free(f_treetypes[i].name);
      mem_free(f_treetypes[i].variantName);
      mem_free(f_treetypes[i].variantPos);
      mem_free(f_treetypes[i].numComponents);
      mem_free(f_treetypes[i].componentName);
      mem_free(f_treetypes[i].componentPos);
      mem_free(f_treetypes[i].componentType);
    }
    mem_free(f_treetypes);
  }

  for (i = 0; i < f_numSs; i++)
    mem_free(tables[i]);
  mem_free(tables);
  return G;
}
