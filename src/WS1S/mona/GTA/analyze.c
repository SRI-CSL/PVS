/* analyze.c */
  
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

#include "../BDD/bdd.h"
#include "gta.h"

/* GLOBAL VARIABLES */

static SsId *leafs_gathered; /* used to collect leafs of a BDD node */
static unsigned leafs_gathered_size; /* space allocated for array above,
					only needed for "invariant" */
static unsigned leafs_gathered_next; /* next available space in leafs_gathered
					for the description of a leaf */
static Tree *all_trees = NULL; /* list of all trees */

static GTA *gta_global; /* for invariant only */

/* DATA STRUCTURES */

Tree *make_tree_leaf(SsId d_, State p) 
{
  Tree *t = (Tree *) mem_alloc(sizeof (Tree));
  t->empty = TRUE;
  t->d = d_;
  t->state = p;
  t->depth = 0;
  t->size = 0;
  t->next = all_trees;
  all_trees = t;
  return t;
}

Tree *make_tree_internal(Tree *left1, Tree *right1, SsId d_, State p,
			 bdd_manager *bddm_, bdd_handle behavior_handle_) 
{
  Tree *t = (Tree *) mem_alloc(sizeof (Tree));
  t->empty = FALSE;
  t->bddm = bddm_;
  t->behavior_handle = behavior_handle_;
  t->d = d_;
  t->state = p;
  t->left = left1;
  t->right = right1;
  t->depth = 1+((left1->depth>right1->depth)?left1->depth:right1->depth);
  t->size = 1+left1->size+right1->size;
  t->next = all_trees;
  all_trees = t;
  return t;
}
  

/* FUNCTIONS */

void leaf_gather_fn(unsigned val) 
{
  invariant(leafs_gathered_next < leafs_gathered_size);
  leafs_gathered[leafs_gathered_next++] = val;
}

/* calculate states (leaves) reachable from ptr; if ptr has already
   been involved in a states_reachable calculation, then no states are
   returned. Note: "*leafs" is changed to NULL or a freshly allocated
   array with leafs_length number of fresh leafs */
void states_reachable(StateSpace *ss,
		      bdd_ptr ptr,
		      unsigned **leafs,
		      unsigned *leafs_length) 
{
  if (!bdd_mark(ss->bddm, ptr)) {
    leafs_gathered = (State *) mem_alloc(sizeof(State) * ss->size); 
    leafs_gathered_size = ss->size;
    leafs_gathered_next = 0;
    bdd_call_leafs(ss->bddm, ptr, &leaf_gather_fn);
    *leafs = leafs_gathered;
    *leafs_length = leafs_gathered_next;
  }
  else {
    *leafs = NULL;
    *leafs_length = 0;
  }
}

void print_tree(Tree *t, unsigned no, unsigned *free_map) 
{
  invariant(t);

  if (t->empty)
    printf("()");
  else {
    printf("(");
    print_one_path(bdd_roots(t->bddm)[t->behavior_handle], /* the BDD node */
		   t->state,   /* the leaf (state to be reached) */
		   t->bddm,   /* the BDD manager */
		   no, 
		   free_map);
    printf(",");
    print_tree(t->left,no,free_map);
    printf(",");
    print_tree(t->right,no,free_map);
    printf(")");
  }
}

void print_tree_graphviz(Tree *t, unsigned no, 
			 unsigned *free_map, int num)
{
  if (t->empty)
    printf(" N%dN%d [label = \"%s: -\"];\n", t->d, num, guide.ssName[t->d]);
  else {
    printf(" N%dN%d [label = \"%s: ", t->d, num, guide.ssName[t->d]);
    print_one_path(bdd_roots(t->bddm)[t->behavior_handle], /* the BDD node */
		   t->state,   /* the leaf (state to be reached) */
		   t->bddm,   /* the BDD manager */
		   no, 
		   free_map);
    printf("\"];\n");
    print_tree_graphviz(t->left, no, free_map, 2*num);
    printf(" N%dN%d -> N%dN%d;\n", t->d, num, t->left->d, 2*num);
    print_tree_graphviz(t->right, no, free_map, 2*num+1);
    printf(" N%dN%d -> N%dN%d;\n", t->d, num, t->right->d, 2*num+1);
  }
}
  
/* UNPROCESSED AND MORE GLOBALS */

static State **unprocessed; /* unprocessed[d][i], i < next[d]
			       is a state in state space d */
static unsigned *next; /* next[d]= index of next state to be inserted in
			  unprocessed[d] */
static unsigned *done; /* done[d]= index of oldest state in d that
			  has been processed; done[d] <= next[d] */

static void unprocessed_push(SsId d, State p) 
{
  invariant(next[d] < gta_global->ss[d].size);
  unprocessed[d][next[d]++] = p;
}

static State unprocessed_get(SsId d) 
{
  invariant(done[d] <= next[d]);
  return unprocessed[d][done[d]++];
}

static boolean unprocessed_is_empty(SsId d) 
{
  invariant(done[d] <= next[d]);
  return (done[d] == next[d]);
}

/* UPDATE TREE */

/* see if (left, right) is a less deep alternative
   to res; if yes, update res.  We do not in this
   version update further all tree records that
   in turn has res as a left or right descendant. If we
   did this recursively, an algorithm calculating a
   mininum (depth-wise) counter example would emerge */

void update_tree(Tree *res,
		 Tree *left,
		 Tree *right,
		 SsId d,
		 State p,
		 bdd_manager *bddm,
		 bdd_handle behavior_handle) 
{
  unsigned l_d = left->depth + 1;
  unsigned r_d = right->depth + 1; 
  if ((l_d < res->depth) &&
      (r_d < res->depth)) {
    res->depth = 
      (l_d < r_d)? r_d : l_d;
    res->state = p;
    res->left = left;
    res->right = right;
    res->bddm = bddm;
    res->d = d;
    res->behavior_handle = behavior_handle;
  }
}

/* CALCULATE EXAMPLES */

Tree *gtaMakeExample(GTA *gta, int status) 
{  
  Tree ***sample_tree; /* sample_tree[d][p] is a pointer to a
       				 tree that brings the automaton in state p */
  SsId d;
  State p;

  sample_tree =  (Tree ***) 
    mem_alloc(sizeof(Tree**) * guide.numSs);

  gta_global = gta; /* for invariant purposes */

  unprocessed = (State **) mem_alloc(sizeof(State *) * guide.numSs);
  next = (unsigned *) mem_alloc(sizeof (unsigned) * guide.numSs);
  done = (unsigned *) mem_alloc(sizeof (unsigned) * guide.numSs);

  /* initialize sample_tree so that sample_tree[d][p] is the empty
     tree for initial states of state spaces in universes and null
     otherwise */

  for (d = 0; d < guide.numSs; d++) {
    next[d] = 0;
    done[d] = 0;
    unprocessed[d] = (State *) mem_alloc(sizeof (State) * gta->ss[d].size);
    sample_tree[d] = 
      (Tree **) mem_alloc(sizeof (Tree *) * gta->ss[d].size);
    for (p=0; p < gta->ss[d].size; p++) 
      if (p == gta->ss[d].initial && guide.ssUniv[d] >= 0) {
	/* p is initial and not in the hat of the universe embedding
           into the infinite binary tree */
	sample_tree[d][p] = make_tree_leaf(d,p);
	unprocessed_push(d,p);
      }
      else
	sample_tree[d][p] = NULL;
    bdd_prepare_apply1(gta->ss[d].bddm); 
  }
 
  { 
    SsId d_hat;
    State p_hat;
    unsigned i, j;
    d_hat = 0;
    while (d_hat < guide.numSs)
      if (!unprocessed_is_empty(d_hat)) {
	p_hat = unprocessed_get(d_hat);

	/* regard p_hat under the left view, i.e., study all p2 in appropriate
	   state spaces such that a transition function is defined for
	   (p_hat, p2) */

      	for (j=0; j < guide.numHitsLeft[d_hat]; j++) {
	  SsId d = guide.hitsLeft[d_hat][j];
	  SsId d2;
	  State p2;
	  StateSpace *ss_ptr = &gta->ss[d];
	  d2 = guide.muRight[d];
	  for (p2 = 0; p2 < ss_ptr->rs; p2++)
	    if (sample_tree[d2][p2] != NULL) {
	      bdd_handle behavior_handle = 
		ss_ptr->behaviour[p_hat * ss_ptr->rs + p2];
	      State *new_states;
	      unsigned new_states_size;
	    
	      states_reachable(ss_ptr,
			       BDD_ROOT(ss_ptr->bddm, behavior_handle),
			       &new_states,
			       &new_states_size);
	 
	      for (i = 0; i < new_states_size; i++) {
		State new_state = new_states[i];
	    
		/* have we gotten a new tree? */
		if (sample_tree[d][new_state] == NULL) {
		  sample_tree[d][new_state] = 
		    make_tree_internal(sample_tree[d_hat][p_hat],
				       sample_tree[d2][p2],
				       d,
				       new_state,
				       ss_ptr->bddm,
				       behavior_handle);
		  unprocessed_push(d, new_state);
		} else 
		  update_tree(sample_tree[d][new_state],
			      sample_tree[d_hat][p_hat],
			      sample_tree[d2][p2],
			      d,
			      new_state,
			      ss_ptr->bddm,
			      behavior_handle);
	      }
	      if (new_states != NULL) free(new_states);
	    }
	}
      
	/* now regard p_hat under the right view */
	
	for (j=0; j < guide.numHitsRight[d_hat]; j++) {
	  SsId d = guide.hitsRight[d_hat][j];
	  SsId d1;
	  State p1;
	  StateSpace *ss_ptr = &gta->ss[d];
	  d1 = guide.muLeft[d];
	  for (p1 = 0; p1 < ss_ptr->ls; p1++) 
	    if (sample_tree[d1][p1] != NULL) {
	      bdd_handle behavior_handle = 
		ss_ptr->behaviour[p1 * ss_ptr->rs + p_hat]; 
	      State *new_states;
	      unsigned new_states_size;
	  
	      states_reachable(ss_ptr,
			       BDD_ROOT(ss_ptr->bddm,behavior_handle),
			       &new_states,
			       &new_states_size);
	      
	      for (i = 0; i < new_states_size; i++) {
		State new_state = new_states[i];
	    
		if (sample_tree[d][new_state] == NULL) {
		  sample_tree[d][new_state] = 
		    make_tree_internal(sample_tree[d1][p1],
				       sample_tree[d_hat][p_hat],
				       d,
				       new_state,
				       ss_ptr->bddm,
				       behavior_handle);
		  unprocessed_push(d, new_state);		  
		} else
		  update_tree(sample_tree[d][new_state],
			      sample_tree[d1][p1],
			      sample_tree[d_hat][p_hat],
			      d,
			      new_state,
			      ss_ptr->bddm,
			      behavior_handle);
	      }
	      if (new_states != NULL) free(new_states);
	    }
	}
	d_hat = 0; /* we try the lowest numbered state space first */
      }
      else /* d_hat seems to be done */
	d_hat++;
    /* while loop ends here */

  }
  /* look for a final state or non-final state reached, as determined
     by the status, (not every such state is interesting, only one
     that is actually reachable by a tree that fully contains the hat,
     i.e., one described by sample_tree */
  {
    State p = 0;
    Tree *result = NULL;
    p = 0;
    while (p < gta->ss[0].size) {
      /* since only trees reachable from outside the hat are
	 mentioned, there may be states in the root state space that
	 have NULL entries---so check */
      if (sample_tree[0][p] != NULL) { 
	if (gta->final[p] == status ) {
	  result = sample_tree[0][p];
	  break; 
	}
      }
      p++;
    }
    /* remove table sample_tree */
    for (d=0; d < guide.numSs; d++) {
      if (sample_tree[d] != NULL)
	free(sample_tree[d]);
      else
	printf("internal error\n");
    };
    free(sample_tree);
    /* the trees themselves must be deleted later */

    for (d = 0; d < guide.numSs; d++) {
      free(unprocessed[d]);
    }
    free(unprocessed);
    free(next);
    free(done);
    return result;
  }
}
  
void print_universes(Tree *example, int no_of_free, unsigned *free_map) 
{
  if (guide.ssUniv[example->d] >= 0) {
    printf("Universe %s:\n", guide.univName[guide.ssUniv[example->d]]);
    print_tree(example, no_of_free, free_map);
    printf("\n");
  }
  else {
    print_universes(example->left, no_of_free, free_map);
    print_universes(example->right, no_of_free, free_map);
  }
}

void print_universes_graphviz(Tree *example, int no_of_free, 
			      unsigned *free_map) 
{
  if (guide.ssUniv[example->d] >= 0) {
    print_tree_graphviz(example, no_of_free, free_map, 1);
    printf(" node [label = \"%s\"]; N%d;\n"
	   " N%d -> N%dN1 [style = dotted];\n", 
	   guide.univName[guide.ssUniv[example->d]], 
	   example->d, example->d, example->d);
    printf(" L -> N%d [style = invis];\n", example->d);
  }
  else {
    print_universes_graphviz(example->left, no_of_free, free_map);
    print_universes_graphviz(example->right, no_of_free, free_map);
  }

}

void print_example_graphviz(Tree *example, int no_of_free, 
			    char **free_var_names, unsigned *free_map, 
			    char *title, char *empty)
{
  int i; 

  printf("digraph MONA_TREE {\n"
	 " center = true;\n"
	 " size = \"7.5,10.5\";\n"
	 " node [shape = plaintext, fontname = Courier];\n");
  if (example) {
    printf(" node [label = \"%s\\n\\nFree variables are: ", title);
    for (i = 0; i < no_of_free; i++)
      printf("%s%s", free_var_names[i], (i == no_of_free -1 ? "": ", "));
    printf("\\n\\nBooleans: ");
    print_one_path(BDD_ROOT(example->bddm,
			    example->behavior_handle), 
		   example->state,  
		   example->bddm,  
		   no_of_free, 
		   free_map);
    printf("\"]; L;\n"
	   " edge [dir = none];\n");
    print_universes_graphviz(example, no_of_free, free_map);
  }
  else
    printf(" node [label = \"Formula is %s\"]; X;\n", empty); 
  printf("}\n");
}


/* print counterexample to formula expressed by gtaimpl (or print
   "valid" if this is the case); if gtaConj is non-NULL, print a
   satisfying example, (if no example, write "unsatisfiable") */

void gtaAnalyze(GTA *gtaImpl, GTA *gtaConj,
		unsigned no_of_free, 
		char **free_var_names, unsigned *free_map, 
		int opt_gs, int opt_gc) 
{
  unsigned i;
  Tree *counterexample, *satisfyingexample;

  counterexample = gtaMakeExample(gtaImpl, -1);
  satisfyingexample = gtaMakeExample(gtaConj, 1);

  if (opt_gs || opt_gc) {
    if (opt_gc)
      print_example_graphviz(counterexample, no_of_free, free_var_names, 
			     free_map, "COUNTER-EXAMPLE", "valid");
    if (opt_gs)
      print_example_graphviz(satisfyingexample, no_of_free, free_var_names, 
			     free_map, "SATISFYING EXAMPLE", "unsatisfiable");
  }
  else {
    if (!counterexample)      
      printf("Formula is valid\n");
    else if (!satisfyingexample)      
      printf("Formula is unsatisfiable\n");
    
    if (counterexample) {
      if (!satisfyingexample)
	printf("\n");
      printf("Free variables are: ");
      for (i = 0; i < no_of_free; i++)
	printf("%s%s", free_var_names[i], (i == no_of_free -1 ? "": ", "));
      printf("\n\n");
      
      printf("A counter-example (for assertion => main) is:\n");
      if (!counterexample->empty) {
	printf("Booleans:\n");
	print_one_path(BDD_ROOT(counterexample->bddm,
				counterexample->behavior_handle), 
		       counterexample->state,  
		       counterexample->bddm,  
		       no_of_free, 
		       free_map);
      }
      printf("\n");
      print_universes(counterexample, no_of_free, free_map);
      printf("\n");
    }
    
    if (satisfyingexample) {
      if (!counterexample) {
	printf("\nFree variables are: ");
	for (i = 0; i < no_of_free; i++)
	  printf("%s%s", free_var_names[i], (i == no_of_free -1 ? "": ", "));
	printf("\n\n");
      }
      
      printf("A satisfying example (for assertion & main) is:\n");
      if (!satisfyingexample->empty) {
	printf("Booleans:\n");
	print_one_path(BDD_ROOT(satisfyingexample->bddm,
				satisfyingexample->behavior_handle), 
		       satisfyingexample->state,  
		       satisfyingexample->bddm,  
		       no_of_free, 
		       free_map);
      }
      printf("\n");
      print_universes(satisfyingexample, no_of_free, free_map);
      printf("\n");
    }
  }
  {/* remove trees */
    Tree *tree_pointer;
    while (all_trees) {
      tree_pointer = all_trees->next;
      free(all_trees);
      all_trees = tree_pointer;
    }
  }
}

