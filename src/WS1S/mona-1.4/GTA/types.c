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
#include <string.h>
#include "gta.h"
#include "../Mem/mem.h"

gtaType *treetypes = 0;
int num_types = 0;

static int currentType = -1, currentVariant, currentComponent;

static void printTypePositions(Tree *tree, unsigned index, 
			       int *first, int begin, int all, char path[],
			       int type);
static Tree *printTreeRoot(Tree *tree, unsigned index, int type, char *path);

/* TREE TYPE INITIALIZATION */

void initTreetypes(int num)
{
  num_types = num;
  treetypes = (gtaType *) mem_alloc(sizeof(gtaType)*num_types);
}

int initTreetype(char *name, int numVariants)
{
  currentType++;
  treetypes[currentType].name = name;
  treetypes[currentType].numVariants = numVariants; 
  treetypes[currentType].variantName = 
    (char **) mem_alloc(sizeof(char *)*numVariants);
  treetypes[currentType].variantPos = 
    (char **) mem_alloc(sizeof(char *)*numVariants);
  treetypes[currentType].numComponents = 
    (int *) mem_alloc(sizeof(int)*numVariants);
  treetypes[currentType].componentName = 
    (char ***) mem_alloc(sizeof(char **)*numVariants);
  treetypes[currentType].componentPos = 
    (char ***) mem_alloc(sizeof(char **)*numVariants);
  treetypes[currentType].ct = 
    (char ***) mem_alloc(sizeof(char **)*numVariants);
  treetypes[currentType].componentType = 
    (int **) mem_alloc(sizeof(int *)*numVariants);
  currentVariant = -1;
  return currentType;
}

void initTreetypeVariant(char *name, int numComponents)
{
  currentVariant++;
  treetypes[currentType].variantName[currentVariant] = name;
  treetypes[currentType].numComponents[currentVariant] = numComponents;
  treetypes[currentType].componentName[currentVariant] =
    (char **) mem_alloc(sizeof(char *)*numComponents);
  treetypes[currentType].componentPos[currentVariant] =
    (char **) mem_alloc(sizeof(char *)*numComponents);
  treetypes[currentType].componentType[currentVariant] =
    (int *) mem_alloc(sizeof(int)*numComponents);
  treetypes[currentType].ct[currentVariant] =
    (char **) mem_alloc(sizeof(char *)*numComponents);
  currentComponent = -1;
}

void setTreetypeComponent(char *name, char *type)
{
  currentComponent++;
  treetypes[currentType].componentName[currentVariant][currentComponent] =
    name;
  treetypes[currentType].ct[currentVariant][currentComponent] = type;
}

void setVariantPos(int type, int var, char *pos)
{
  treetypes[type].variantPos[var] = pos;
}

void setComponentPos(int type, int var, int comp, char *pos)
{
  treetypes[type].componentPos[var][comp] = pos;
}

void setComponentTypes()
{
  int t, v, c, u;
  for (t = 0; t < num_types; t++) {
    for (v = 0; v < treetypes[t].numVariants; v++)
      for (c = 0; c < treetypes[t].numComponents[v]; c++) {
	int found = 0;
	for (u = 0; u < num_types && !found; u++)
	  if (treetypes[t].ct[v][c] == treetypes[u].name) {
	    treetypes[t].componentType[v][c] = u;
	    found = 1;
	  }
	if (!found) 
	  invariant(0);
      }
  }
}

void freeTreetypes()
{
  int t, v;
  for (t = 0; t < num_types; t++) {
    for (v = 0; v < treetypes[t].numVariants; v++) {
      mem_free(treetypes[t].componentType[v]);
      mem_free(treetypes[t].componentName[v]);
      mem_free(treetypes[t].componentPos[v]);
      if (treetypes[t].ct)
	mem_free(treetypes[t].ct[v]);
    }
    mem_free(treetypes[t].componentType);
    mem_free(treetypes[t].componentName);
    mem_free(treetypes[t].componentPos);
    mem_free(treetypes[t].variantName);
    mem_free(treetypes[t].variantPos);
    if (treetypes[t].ct)
      mem_free(treetypes[t].ct);
    mem_free(treetypes[t].numComponents);
  } 
  mem_free(treetypes);
}

/* PRINT TYPED TREE */

static int findVariant(Tree *tree, int num, int idx, 
		       Tree **node, unsigned index)
{ /* find the (unique) leaf node with a 1 */
  int v = -1;
  if (tree && !tree->empty) {
    if (num > 1) { /* search the variant-tree */
      v = findVariant(tree->left, (num+1)/2, idx, node, index);
      if (v == -1)
	v = findVariant(tree->right, num/2, idx+(num+1)/2, node, index);
    }
    else {
      trace_descr t, p = 
	find_one_path(tree->bddm,
		      bdd_roots(tree->bddm)[tree->behavior_handle], 
		      tree->state);
      t = p; 
      while (t && (t->index != index))
	t = t->next;
      if (t && t->value) /* read a 1? */
	v = idx; /* found the variant */
      kill_trace(p);
      *node = tree; /* remember the node */
    }
  }
  return v;
}

static Tree *findComponent(Tree *tree, int num, int c)
{ /* find the c'th leaf node in tree with num leaves */
  while (num > 1) {
    if (tree->empty)
      return 0;
    if (c < (num+1)/2) {
      tree = tree->left; 
      num = (num+1)/2;
    }
    else {
      tree = tree->right;
      c = c - (num+1)/2;
      num = num/2;
    }
  }
  return tree;
}

static void printTypedTree(int type, Tree *tree, unsigned index)
{
  int v, c;
  Tree *node;
  int numComp;
  if (!tree) {
    printf("?");
    return;
  }
  invariant(type != -1);
  invariant(guide.ssUniv[tree->d] >= 0);
  v = findVariant(tree, treetypes[type].numVariants, 0, &node, index);
  if (v == -1) {
    printf("?");
    return;
  }
  printf(treetypes[type].variantName[v]);
  numComp = treetypes[type].numComponents[v];
  if (numComp > 0) {
    printf("(");
    if (numComp > 1) {
      for (c = 0; c < numComp; c++) {
	if (c != 0)
	  printf(",");
	printTypedTree(treetypes[type].componentType[v][c], 
		       findComponent(node, numComp, c),
		       index);
      }
    }
    else /* only one component, dummy on right */
      printTypedTree(treetypes[type].componentType[v][0], 
		     node->left,
		     index);
    printf(")");
  }
}

/* FOLLOW PATH */

static Tree *findNode(Tree *tree, char *pos, int i)
{
  while (tree && pos[i]) {
    switch (pos[i])
      {
      case '0':
	tree = tree->left;
	break;
      case '1':
	tree = tree->right;
	break;
      }
    i++;
  }
  return tree;
}

/* PRINT TREE ROOT */

static Tree *printTreeRootComps(Tree *tree, int num, int idx, unsigned index,
				char path[], int type, int variant)
{
  Tree *res = 0;
  if (tree && !tree->empty) {
    if (num > 1) { 
      res = printTreeRootComps(tree->left, (num+1)/2, idx, index, 
			       path, type, variant);
      if (!res)
	res = printTreeRootComps(tree->right, num/2, idx+(num+1)/2, index, 
				 path, type, variant);
    }
    else if (num == 1) {
      char *component = treetypes[type].componentName[variant][idx];
      char *newpath = (char *) mem_alloc(strlen(path)+strlen(component)+2);
      sprintf(newpath, "%s%s.", path, component);
      res = printTreeRoot(tree, index, 
			  treetypes[type].componentType[variant][idx],
			  newpath);
      mem_free(newpath);
    }
  }
  return res;
}

static Tree *printTreeRootVariants(Tree *tree, int num, int idx, 
				   unsigned index,
				   char path[], int type)
{
  Tree *res = 0;
  if (tree && !tree->empty) {
    if (num > 1) { 
      res = printTreeRootVariants(tree->left, (num+1)/2, idx, index, 
				  path, type);
      if (!res)
	res = printTreeRootVariants(tree->right, num/2, idx+(num+1)/2, index, 
				    path, type);
    }
    else {
      int n = treetypes[type].numComponents[idx];
      char *variant = treetypes[type].variantName[idx];
      char *newpath = (char *) mem_alloc(strlen(path)+strlen(variant)+2);
      sprintf(newpath, "%s%s.", path, variant);
      res = printTreeRootComps(tree->left, (n+1)/2, 0, index,
			       newpath, type, idx);
      if (!res)
	res = printTreeRootComps(tree->right, n/2, (n+1)/2, index,
				 newpath, type, idx);
      mem_free(newpath);
    }
  }
  return res;
}

static Tree *printTreeRoot(Tree *tree, unsigned index, int type, char *path)
{
  Tree *res = 0;
  if (tree && !tree->empty) {
    trace_descr t, p = 
      find_one_path(tree->bddm,
		    bdd_roots(tree->bddm)[tree->behavior_handle], 
		    tree->state);
    t = p; 
    while (t && (t->index != index))
      t = t->next;
    if (t && t->value) { /* read a 1? */
      printf(path);
      res = tree;
    }
    kill_trace(p);
    if (!res)
      res = printTreeRootVariants(tree, treetypes[type].numVariants, 0, 
				  index, path, type);
  }
  return res;
}

/* PRINT POSITION(S) */

static void printTypePosComps(Tree *tree, int num, int idx, unsigned index,
			      int *first, int begin, int all, char path[],
			      int type, int variant)
{
  if (tree && !tree->empty) {
    if (num > 1) { 
      printTypePosComps(tree->left, (num+1)/2, idx, 
			index, first, begin, all, path, type, variant);
      printTypePosComps(tree->right, num/2, idx+(num+1)/2, 
			index, first, begin, all, path, type, variant);
    }
    else if (num == 1) {
      char *component = treetypes[type].componentName[variant][idx];
      char *newpath = (char *) mem_alloc(strlen(path)+strlen(component)+2);
      sprintf(newpath, "%s%s%s", path, begin ? "" : ".", component);
      printTypePositions(tree, index, first, 0, all, newpath,
			 treetypes[type].componentType[variant][idx]);
      mem_free(newpath);
    }
  }
}

static void printTypePosVariants(Tree *tree, int num, int idx, unsigned index,
				 int *first, int begin, int all, char path[],
				 int type)
{
  if (tree && !tree->empty) {
    if (num > 1) { 
      printTypePosVariants(tree->left, (num+1)/2, idx, 
			   index, first, begin, all, path, type);
      printTypePosVariants(tree->right, num/2, idx+(num+1)/2, 
			   index, first, begin, all, path, type);
    }
    else {
      int n = treetypes[type].numComponents[idx];
      char *variant = treetypes[type].variantName[idx];
      char *newpath = (char *) mem_alloc(strlen(path)+strlen(variant)+2);
      sprintf(newpath, "%s%s%s", path, begin ? "" : ".", variant);
      printTypePosComps(tree->left, (n+1)/2, 0, 
			index, first, 0, all, newpath, type, idx);
      printTypePosComps(tree->right, n/2, (n+1)/2, 
			index, first, 0, all, newpath, type, idx);
      mem_free(newpath);
    }
  }
}

static void printTypePositions(Tree *tree, unsigned index, 
			       int *first, int begin, int all, char path[],
			       int type)
{
  if (tree && !tree->empty) {
    trace_descr t, p = 
      find_one_path(tree->bddm,
		    bdd_roots(tree->bddm)[tree->behavior_handle], 
		    tree->state);
    t = p; 
    while (t && (t->index != index))
      t = t->next;
    if (t && t->value) { /* read a 1? */
      if (!*first)
	printf(",");
      printf(path);
      *first = 0;
    }
    kill_trace(p);
    if (all || *first) 
      printTypePosVariants(tree, treetypes[type].numVariants, 0, 
			   index, first, begin, all, path, type);
  }
}

/* PRINT EXAMPLE */

static void printTypeExample(Tree *example, unsigned num, char *names[], 
			     char orders[], unsigned indices[], 
			     int *univs[], int trees[])
{
  trace_descr bools;
  int i;

  bools = find_one_path(example->bddm,
			BDD_ROOT(example->bddm,
				 example->behavior_handle), 
			example->state);
  for (i = 0; i < num; i++) {
    printf(" %s = ", names[i]);
    switch (orders[i]) {
    case 0: /* Boolean variable */
      { 
	trace_descr t = bools;
	while (t && (t->index != indices[i]))
	  t = t->next;
	if (t && t->value) 
	  printf("true\n");
	else 
	  printf("false\n");
	break;
      }
    case 1: /* first-order variable */
      {
	int j, first = 1, any = 0;
	for (j = 0; univs[i][j] != -1; j++) {
	  int u = univs[i][j];
	  Tree *t = findNode(example, guide.univPos[u], 0);
	  if (t) {
	    char *univname = guide.univName[u];
	    char *path = (char *) mem_alloc(strlen(univname)+2);
	    sprintf(path, "%s:", univname);
	    printTypePositions(t, indices[i], &first, 1, 0, path,
			       guide.ssType[t->d]);
	    if (!first)
	      any = 1;
	    mem_free(path);
	  }
	}
	if (!any)
	  printf("?");
	printf("\n");
	break;
      }
    case 2: /* second-order variable */
      {
	if (trees[i]) { /* print as typed tree */
	  int j, any = 0;
	  for (j = 0; univs[i][j] != -1 && !any; j++) {
	    int u = univs[i][j];
	    Tree *t = findNode(example, guide.univPos[u], 0);
	    if (t) {
	      char *univname = guide.univName[u];
	      char *path = (char *) mem_alloc(strlen(univname)+2);
	      sprintf(path, "%s:", univname);
	      t = printTreeRoot(t, indices[i], guide.ssType[t->d], path);
	      mem_free(path);
	      if (t) {
		printTypedTree(guide.ssType[t->d], t, indices[i]);
		any = 1;
	      }
	    }
	  }
	  if (!any)
	    printf("?");
	  printf("\n");
	}
	else { /* print as set of positions */
	  int j, first = 1;
	  printf("{");
	  for (j = 0; univs[i][j] != -1; j++) {
	    int u = univs[i][j];
	    Tree *t = findNode(example, guide.univPos[u], 0);
	    if (t) {
	      char *univname = guide.univName[u];
	      char *path = (char *) mem_alloc(strlen(univname)+2);
	      sprintf(path, "%s:", univname);
	      printTypePositions(t, indices[i], &first, 1, 1, path,
				 guide.ssType[t->d]);
	      mem_free(path);
	    }
	  }
	  printf("}\n");
	}
	break;
      }
    }
  }
  kill_trace(bools);
}

/* ANALYZE WITH TYPES */

void gtaTypeAnalyze(GTA *a, unsigned num, char *names[], char orders[],
		    unsigned indices[], int *univs[], int trees[])
{
  Tree *counterexample, *satisfyingexample;

  counterexample = gtaMakeExample(a, -1);
  satisfyingexample = gtaMakeExample(a, 1);

  if (!counterexample && satisfyingexample)
    printf("Formula is valid\n");
  else if (!satisfyingexample)      
    printf("Formula is unsatisfiable\n");
  
  if (counterexample) {
    printf("A counter-example is:\n");
    printTypeExample(counterexample, num, names, orders, 
		     indices, univs, trees);
  }
  if (satisfyingexample) {
    if (counterexample)
      printf("\n");
    printf("A satisfying example is:\n");
    printTypeExample(satisfyingexample, num, names, orders, 
		     indices, univs, trees);
  }
  gtaFreeTrees();
}
