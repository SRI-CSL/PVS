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
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "gtalib.h"

mgGta *mgLoad(char *filename)
{
  FILE *file;
  mgGta *gta;
  mgId i, l, r;
  mgState j;
  mNode n;
  unsigned t;
  char buffer[1000], buffer2[1000];
  int i1, i2, i3;

  if ((file = fopen(filename, "r")) == 0)
    return 0;
  gta = (mgGta *) memset(malloc(sizeof(mgGta)), 0, sizeof(mgGta));
  if (fscanf(file,
	     "MONA GTA\n"
	     "number of variables: %u\n"
	     "state spaces: %u\n"
	     "universes: %u\n"
             "state space sizes:",
	     &gta->numVars, &gta->numSS, &gta->numUnivs) != 3) {
    fclose(file);
    mgFree(gta);
    return 0;
  }
  
  gta->stateSpace = (mgStateSpace *) memset(malloc(sizeof(mgStateSpace)*gta->numSS),
					    0, sizeof(mgStateSpace)*gta->numSS);
  for (i = 0; i < gta->numSS; i++)
    if (fscanf(file, " %u", &gta->stateSpace[i].numStates) != 1) {
      fclose(file);
      mgFree(gta);
      return 0;
    }
  fscanf(file, "\nfinal:");
  gta->final = (mgKind *) malloc(sizeof(mgKind)*gta->stateSpace[0].numStates);
  for (i = 0; i < gta->stateSpace[0].numStates; i++) {
    int f;
    fscanf(file, " %d", &f);
    gta->final[i] = f;
  }
  fscanf(file, "\nguide:\n");
  for (i = 0; i < gta->numSS; i++) {
    if (fscanf(file, "%s %u %u\n", 
	       buffer, &gta->stateSpace[i].leftSS, 
	       &gta->stateSpace[i].rightSS) != 3) {
      fclose(file);
      mgFree(gta);
      return 0;
    }
    gta->stateSpace[i].name = (char *) malloc(strlen(buffer)+1);
    strcpy(gta->stateSpace[i].name, buffer);
  }

  if (fscanf(file, "\ntypes: %u\n", &gta->numTypes) != 1) {
    fclose(file);
    mgFree(gta);
    return 0;
  }
  gta->type = (mgType *) malloc(sizeof(mgType)*gta->numTypes);
  for (i1 = 0; i1 < gta->numTypes; i1++) {
    mgType *t = &gta->type[i1];
    fscanf(file, "\ntype: %s\n", buffer);
    t->name = (char *) malloc(strlen(buffer)+1);
    strcpy(t->name, buffer);
    fscanf(file, "\nvariants: %u\n", &t->numVariants);
    t->variant = 
      (mgTypeVariant *) malloc(sizeof(mgTypeVariant)*t->numVariants);
    for (i2 = 0; i2 < t->numVariants; i2++) {
      mgTypeVariant *v = &t->variant[i2];
      fscanf(file, "\nvariant: %s ", buffer);
      v->name = (char *) malloc(strlen(buffer)+1);
      strcpy(v->name, buffer);
      fscanf(file, "%s\n", buffer);
      if (buffer[0] == '-')
	buffer[0] = 0;
      v->pos = (char *) malloc(strlen(buffer)+1);
      strcpy(v->pos, buffer);
      fscanf(file, "\ncomponents: %u\n", &v->numComponents);
      v->component = 
	(mgTypeComponent *) malloc(sizeof(mgTypeComponent)*v->numComponents);
      for (i3 = 0; i3 < v->numComponents; i3++) {
	mgTypeComponent *c = &v->component[i3];
	fscanf(file, "\n%s", buffer);
	c->name = (char *) malloc(strlen(buffer)+1);
	strcpy(c->name, buffer);
	fscanf(file, " %s ", buffer);
	if (buffer[0] == '-')
	  buffer[0] = 0;
	c->pos = (char *) malloc(strlen(buffer)+1);
	strcpy(c->pos, buffer);
	fscanf(file, "%u\n", &c->type);
      }
    }
  }
  
  fscanf(file, "\nuniverses:\n");
  gta->universe = (mgUniverse *) memset(malloc(sizeof(mgUniverse)*gta->numUnivs),
					0, sizeof(mgUniverse)*gta->numUnivs);
  for (i = 0; i < gta->numUnivs; i++) {
    if (fscanf(file, "%s %s\n", buffer, buffer2) != 2) {
      fclose(file);
      mgFree(gta);
      return 0;
    }
    gta->universe[i].name = (char *) malloc(strlen(buffer)+1);
    strcpy(gta->universe[i].name, buffer);
    gta->universe[i].pos = (char *) malloc(strlen(buffer2)+1);
    strcpy(gta->universe[i].pos, buffer2);
  }
  
  fscanf(file, "\nvariable orders and state spaces:\n");
  gta->var = (mgVariable *) memset(malloc(sizeof(mgVariable)*gta->numVars),
				   0, sizeof(mgVariable)*gta->numVars);
  for (i = 0; i < gta->numVars; i++) {
    fscanf(file, " %s %d:", buffer, &gta->var[i].order);
    gta->var[i].name = (char *) malloc(strlen(buffer)+1);
    strcpy(gta->var[i].name, buffer);
    gta->var[i].varSS = (mgId *) malloc(sizeof(mgId)*gta->numSS);
    j = 0;
    while (1) {
      fscanf(file, " %u", &gta->var[i].varSS[j++]);
      if (fgetc(file)=='\n')
	break;
    };
    gta->var[i].numVarSS = j;
    gta->var[i].varSS = (mgId *) realloc(gta->var[i].varSS, sizeof(mgId)*j);
  }
  
  for (i = 0; i < gta->numSS; i++) {
    mgStateSpace *ss = &gta->stateSpace[i];
    fscanf(file, 
	   "\nstate space %u:\n"
	   "initial state: %u\n"
	   "bdd nodes: %u\n",
	   &t, &ss->initial, &ss->numBddNodes);
    fscanf(file, "%s", buffer);
    ss->inhacc = NULL;
    if (strcmp(buffer, "inherited-acceptance:") == 0) {
      unsigned t1, t2, t3;
      ss->inhacc = (mgInhAcc *) malloc(sizeof(mgInhAcc)*ss->numStates);
      for (j = 0; j < ss->numStates; j++) {
	fscanf(file, "\n%u %u %u ", &t1, &t2, &t3);
	ss->inhacc[j].canReject = t1;
	ss->inhacc[j].canDontCare = t2; 
	ss->inhacc[j].canAccept = t3;
      }
      fscanf(file, "behaviour:\n");
    }
    ss->behaviour = 
      (mNode **) malloc(sizeof(mNode *)*gta->stateSpace[ss->leftSS].numStates);
    for (l = 0; l < gta->stateSpace[ss->leftSS].numStates; l++) {
      ss->behaviour[l] = 
	(mNode *) malloc(sizeof(mNode)*gta->stateSpace[ss->rightSS].numStates);
      for (r = 0; r < gta->stateSpace[ss->rightSS].numStates; r++)
	fscanf(file, "%u ", &ss->behaviour[l][r]);
    }
    fscanf(file, "\nbdd:\n");
    ss->bddNode = (mBdd *) malloc(sizeof(mBdd)*ss->numBddNodes);
    for (n = 0; n < ss->numBddNodes; n++)
      fscanf(file, "%i %u %u\n",
	     &ss->bddNode[n].idx,
	     &ss->bddNode[n].lo,
	     &ss->bddNode[n].hi);
  }

  if (fgetc(file) != 'e' ||
      fgetc(file) != 'n' ||
      fgetc(file) != 'd') {
    fclose(file);
    mgFree(gta);
    return 0;
  }
  fclose(file);
  return gta;
}

int mgStore(mgGta *gta, char *filename)
{
  FILE *file;
  mgId i, l, r;
  mgState j;
  mNode n;
  int i1, i2, i3;

  if ((file = fopen(filename, "w")) == 0)
    return 0;

  fprintf(file,
	  "MONA GTA\n"
	  "number of variables: %u\n"
	  "state spaces: %u\n"
	  "universes: %u\n"
	  "state space sizes:",
	  gta->numVars, gta->numSS, gta->numUnivs);
  for (i = 0; i < gta->numSS; i++)
    fprintf(file, " %u", gta->stateSpace[i].numStates);
  fprintf(file, "\nfinal:");
  for (i = 0; i < gta->stateSpace[0].numStates; i++)
    fprintf(file, " %d", gta->final[i]);
  fprintf(file, "\nguide:\n");
  for (i = 0; i < gta->numSS; i++)
    fprintf(file, " %s %u %u\n", gta->stateSpace[i].name,
	    gta->stateSpace[i].leftSS, gta->stateSpace[i].rightSS);
  fprintf(file, "types: %d\n", gta->numTypes);
  for (i1 = 0; i1 < gta->numTypes; i1++) {
    mgType *t = &gta->type[i1];
    fprintf(file, " type: %s\n", t->name);
    fprintf(file, "  variants: %d\n", t->numVariants);
    for (i2 = 0; i2 < t->numVariants; i2++) {
      mgTypeVariant *v = &t->variant[i2];
      fprintf(file, "  variant: %s %s\n", 
	      v->name, v->pos[0] ? v->pos : "-");
      fprintf(file, "  components: %d\n", v->numComponents);
      for (i3 = 0; i3 < v->numComponents; i3++) {
	mgTypeComponent *c = &v->component[i3];
	fprintf(file, "   %s %s %d\n",
		c->name, c->pos[0] ? c->pos : "-", c->type);
      }
    }
  }
  fprintf(file, "universes:\n");
  for (i = 0; i < gta->numUnivs; i++)
    fprintf(file, " %s %s\n", gta->universe[i].name, gta->universe[i].pos);
  fprintf(file, "variable orders and state spaces:\n");
  for (i = 0; i < gta->numVars; i++) {
    fprintf(file, " %s %u:", gta->var[i].name, gta->var[i].order);
    for (j = 0; j < gta->var[i].numVarSS; j++)
      fprintf(file, " %d", gta->var[i].varSS[j]);
    fprintf(file, "\n");
  }

  for (i = 0; i < gta->numSS; i++) {
    fprintf(file, 
	   "\nstate space %u:\n"
	   " initial state: %u\n"
	   " bdd nodes: %u\n",
	   i, gta->stateSpace[i].initial, gta->stateSpace[i].numBddNodes);
    if (gta->stateSpace[i].inhacc) {
      fprintf(file, " inherited-acceptance:\n");
      for (j = 0; j < gta->stateSpace[i].numStates; j++)
	fprintf(file, "  %u %u %u\n",
		gta->stateSpace[i].inhacc[j].canAccept,
		gta->stateSpace[i].inhacc[j].canDontCare,
		gta->stateSpace[i].inhacc[j].canReject);
    }
    fprintf(file, " behaviour:\n ");
    for (l = 0; l < gta->stateSpace[gta->stateSpace[i].leftSS].numStates; l++) {
      for (r = 0; r < gta->stateSpace[gta->stateSpace[i].rightSS].numStates; r++)
	fprintf(file, " %u", gta->stateSpace[i].behaviour[l][r]);
      fprintf(file, "\n ");
    }
    fprintf(file, "bdd:\n");
    for (n = 0; n < gta->stateSpace[i].numBddNodes; n++)
      fprintf(file, "  %i %u %u\n",
	      gta->stateSpace[i].bddNode[n].idx,
	      gta->stateSpace[i].bddNode[n].lo,
	      gta->stateSpace[i].bddNode[n].hi);
  }
  fprintf(file, "\nend\n");
  fclose(file);
  return 1;
}

void mgFree(mgGta *gta)
{
  mgId i, l;
  int i1, i2, i3;
  if (gta->stateSpace) {
    for (i = 0; i < gta->numSS; i++) {
      for (l = 0; l < gta->stateSpace[gta->stateSpace[i].leftSS].numStates; l++)
	free(gta->stateSpace[i].behaviour[l]);
      free(gta->stateSpace[i].behaviour);
      free(gta->stateSpace[i].bddNode);
      free(gta->stateSpace[i].name);
      free(gta->stateSpace[i].inhacc);
    }
    free(gta->stateSpace);
  }
  if (gta->universe) {
    for (i = 0; i < gta->numUnivs; i++) {
      if (gta->universe[i].name)
	free(gta->universe[i].name);
      if (gta->universe[i].pos)
	free(gta->universe[i].pos);
    }
    free(gta->universe);
  }
  if (gta->var) {
    for (i = 0; i < gta->numVars; i++) {
      if (gta->var[i].name)
	free(gta->var[i].name);
      if (gta->var[i].varSS)
	free(gta->var[i].varSS);
    }
    free(gta->var);
  }
  if (gta->final)
    free(gta->final);
  if (gta->numTypes && gta->type) {
    for (i1 = 0; i1 < gta->numTypes; i1++) {
      mgType *t = &gta->type[i1];
      for (i2 = 0; i2 < t->numVariants; i2++) {
	mgTypeVariant *v = &t->variant[i2];
	for (i3 = 0; i3 < v->numComponents; i3++) {
	  mgTypeComponent *c = &v->component[i3];
	  free(c->name);
	  free(c->pos);
	}
	free(v->name);
	free(v->pos);
	free(v->component);
      }
      free(t->name);
      free(t->variant);
    }
    free(gta->type);
  }
  free(gta);
}

mgState mgDelta(mgGta *gta, mgId ss, mgState left, mgState right, mA a)
{
  mNode n = gta->stateSpace[ss].behaviour[left][right];
  mBdd *bdd = gta->stateSpace[ss].bddNode;

  while (bdd[n].idx != -1)
    if (a[bdd[n].idx])
      n = bdd[n].hi;
    else
      n = bdd[n].lo;

  return bdd[n].lo;
}

void mgAssign(mgGta *gta, mgTreeNode *t, mgId id)
{
  if (t) {
    mgState left, right;
    t->id = id;
    mgAssign(gta, t->left, gta->stateSpace[id].leftSS);
    mgAssign(gta, t->right, gta->stateSpace[id].rightSS);
    if (t->left)
      left = t->left->state;
    else
      left = gta->stateSpace[gta->stateSpace[id].leftSS].initial;
    if (t->right)
      right = t->right->state;
    else
      right = gta->stateSpace[gta->stateSpace[id].rightSS].initial;
    t->state = mgDelta(gta, id, left, right, t->a);
  }
}

int mgCheck(mgGta *gta, mgTreeNode *t)
{
  mgAssign(gta, t, 0);
  return gta->final[t->state];
}
