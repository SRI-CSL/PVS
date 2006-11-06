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
#include "../Mem/mem.h"
#include "gta.h"

SsId s, lSs, rSs; /* current, left and right state spaces; s is the
		     state space that has been split, lSs and rSs, the
		     successors of s, are the candidates for further
		     splitting */
GTA *orig, *res; /* original and resulting automata */

unsigned **block; /* block[s][i] is the block number (discriminator)
		     of state i in state space s; block numbers are
		     consecutive, starting at 0 */

unsigned *numBlocks; /* numBlocks[s] is the size of the partition for
			state space s, i.e.  highest block number + 1 */

bdd_ptr *matrix, *transposed; /* normal and transposed behaviour matrix, 
				 note: contains bdd_ptrs not bdd_handles 
				 to obtain fewer cache misses during compare */
enum Candidate {
  cNEVER, 
  cHASBEEN, 
  cYES
} *candidate; /* candidate[s]==cYES: 
		   if state space s has been split,
		   which entails that s's left and right successor
		   spaces, not s itself (unless s has itself as a
		   successor), are candidates for further refinement
		 candidate[s]==cNEVER: 
                   if s never has been split
		 candidate[s]==cHASBEEN: 
                   if s has been refined at
		   least once (s could still have just one block) */

/* sorting */

bdd_ptr **sorted; /* invariant: sorted[i][j] == qm[original[i]*maxSize + j]
		     in fact, sorted[i] == &qm[original[i]*maxSize] */
unsigned *original; /* original[i] = the original number of a row
		       in sorted order */
bdd_ptr *qm; /* matrix to be sorted, for storage allocation
		purposes, row length is assume to be maxSize; actual
		rowlength is given by qcols; also maxSize rows are
		allocated */
unsigned qcols, maxSize; /* used and allocated number of columns
				in matrix qm */
unsigned *qb; /* qb[i] = block[s][original[i]] */

int compare(unsigned i, unsigned j)
{
  int n;

  /* we compare block numbers first, so that all rows in new ordering
     that are in the same old block are consecutive */
  if (qb[i] > qb[j])
    return 1;
  else if (qb[i] < qb[j])
    return -1;
 
 /* compare wrt. matrix rows and previous block assignments */
  for (n = 0; n < qcols; n++)
    if (sorted[i][n] > sorted[j][n])
      return 1;
    else if (sorted[i][n] < sorted[j][n])
      return -1;

  /* i and j belong to the same old block and they have the same behavior
     across the row */
  return 0;
}

void swap(unsigned i, unsigned j)
{
  bdd_ptr *tp;
  unsigned tb, to;

  /* swap 'block' and 'original' along with 'sorted' */
  tp = sorted[i];
  to = original[i];
  tb = qb[i];
  sorted[i] = sorted[j];
  original[i] = original[j];
  qb[i] = qb[j];
  sorted[j] = tp;
  original[j] = to;
  qb[j] = tb;
}

void quicksort(int from, int to) /*** IMPROVE SORTING?!?!? ***/
     /* yes, cryptic */
{
  if (from < to) {
    int i = from, j = to, v = to;
    do {
      while (i < to && compare(i, v) < 0)
	i++;
      while (j > from && compare(j, v) > 0)
	j--;
      if (i <= j) {
	swap(i, j);
	if (v == i)
	  v = j;
	else if (v == j)
	  v = i;
	i++;
	j--;
      }
    } while (i <= j);
    quicksort(from, j);
    quicksort(i, to);
  }
}

void sort(bdd_ptr *m, unsigned *b,
	  unsigned rows, unsigned cols)
{
  int i;
  qm = m; 
  qcols = cols;

  /* initialize unsorted */
  for (i = 0; i < rows; i++) {
    sorted[i] = &m[i*maxSize];
    original[i] = i;
    qb[i] = b[i];
  }

  /* sort rows in m */
  quicksort(0, rows-1);

  /************
    printf("SORT\n");
    for (i = 0; i < rows; i++) {
      for (j = 0; j < cols; j++)
        printf("%-3u ", sorted[i][j]);
      printf("   %-3u\n", b[i]);
    }
    printf("SORTDONE\n");
  *********/
}

/* leaf function */

unsigned fn_block(unsigned p)
{
  return block[s][p];
}

/* main function */

GTA *gtaMinimize(GTA *g)
{
  int done;
  State i, j;
  unsigned p;

  /* initialize */
  orig = g;
  res = gtaMake();
  maxSize = 0;
  block = (unsigned **) mem_alloc(sizeof(unsigned *)*guide.numSs);
  numBlocks = (unsigned *) mem_alloc(sizeof(unsigned)*guide.numSs);
  candidate = (enum Candidate *) mem_alloc(sizeof(enum Candidate)*guide.numSs);

  for (s = 0; s < guide.numSs; s++) {
    block[s] = (unsigned *) mem_alloc(sizeof(unsigned)*orig->ss[s].size);
    if (orig->ss[s].size > maxSize)
      maxSize = orig->ss[s].size;

    /* set up initial partitions and candidates */
    if (s > 0) {
      candidate[s] = cNEVER; /* never processed yet */
      for (i = 0; i < orig->ss[s].size; i++)
	block[s][i] = 0;
      numBlocks[s] = 1;
    }
    else {
      int anyAccept = 0, anyReject = 0, anyDontcare = 0;
      numBlocks[0] = 0;

      for (i = 0; i < orig->ss[0].size; i++) {
	invariant(orig->final[i] >= -1 && orig->final[i] <= 1); 
	if (orig->final[i] == -1) 
	  anyReject = 1;
	if (orig->final[i] == 1) 
	  anyAccept = 1;
	if (orig->final[i] == 0) 
	  anyDontcare = 1;
      }
      if (anyReject)
	anyReject = numBlocks[0]++;
      if (anyAccept)
	anyAccept = numBlocks[0]++;
      if (anyDontcare) 
	 anyDontcare = numBlocks[0]++;
      /* AnyX is the index of the block containing all AnyX states (if any) */
      for (i = 0; i < orig->ss[0].size; i++)
	block[0][i] = (orig->final[i] == -1) ? anyReject  
	  : (orig->final[i] == 1)? anyAccept : anyDontcare;      
      if (numBlocks[0] == 1)
	candidate[0] = cNEVER; /* only one block */
      else
	candidate[0] = cYES; /* initial s.s. is a candidate for splitting */
    }
  }

  /*....optimize case when numBlocks[0]==1...*/

  sorted = (bdd_ptr **) mem_alloc(sizeof(bdd_ptr*)*maxSize);
  matrix = (bdd_ptr *) mem_alloc(sizeof(bdd_ptr)*maxSize*maxSize);
  transposed = (bdd_ptr *) mem_alloc(sizeof(bdd_ptr)*maxSize*maxSize);
  original = (unsigned *) mem_alloc(sizeof(unsigned)*maxSize);
  qb = (unsigned *) mem_alloc(sizeof(unsigned)*maxSize);

  /* refine partitions until fixed point reached */
  do {
    done = 1;

    for (s = 0; s < guide.numSs; s++) 
      if (candidate[s] == cYES) {
	candidate[s] = cHASBEEN;

	lSs = guide.muLeft[s];
	rSs = guide.muRight[s];
	
	/* find behaviour wrt. current partition */
	bdd_prepare_apply1(orig->ss[s].bddm);
	if (res->ss[s].bddm)
	  bdd_kill_manager(res->ss[s].bddm);
	res->ss[s].bddm = 
	  bdd_new_manager(orig->ss[s].bddm->table_elements, 0);

	for (i = 0; i < orig->ss[lSs].size; i++)
	  for (j = 0; j < orig->ss[rSs].size; j++)
	    matrix[i*maxSize + j] = transposed[j*maxSize + i] = 
	      bdd_apply1(orig->ss[s].bddm, 
			 BDD_ROOT(orig->ss[s].bddm,BEH(orig->ss[s], i, j)),
			 res->ss[s].bddm,
			 fn_block);
	      
	/* refine left state space */
	sort(matrix, block[lSs], 
	     orig->ss[lSs].size, orig->ss[rSs].size);
	p = 0;
	for (i = 0; i < orig->ss[lSs].size - 1; i++) {
	  invariant(compare(i, i+1) <= 0);
	  if (compare(i, i+1) != 0)
	    block[lSs][original[i]] = p++;
	  else
	    block[lSs][original[i]] = p;
	}
	block[lSs][original[i]] = p++;
	invariant(p >= numBlocks[lSs]);
	if (p > numBlocks[lSs]) {
	  candidate[lSs] = cYES;
	  done = 0;
	  numBlocks[lSs] = p;
	}
	
	/* refine right state space */
	sort(transposed, block[rSs],
	     orig->ss[rSs].size, orig->ss[lSs].size);
	p = 0;
	for (i = 0; i < orig->ss[rSs].size - 1; i++) {
	  invariant(compare(i, i+1) <= 0);
	  if (compare(i, i+1) != 0)
	    block[rSs][original[i]] = p++;
	  else
	    block[rSs][original[i]] = p;
	}
	block[rSs][original[i]] = p++;
	invariant(p >= numBlocks[rSs]);
	if (p > numBlocks[rSs]) {
	  candidate[rSs] = cYES;
	  done = 0;
	  numBlocks[rSs] = p;
	}
      }
  } while (!done);
  
  /* build result automaton */
  for (s = 0; s < guide.numSs; s++) {
    lSs = guide.muLeft[s];
    rSs = guide.muRight[s];
    
    if (candidate[lSs] == cNEVER)
      res->ss[s].ls = 1;
    else
      res->ss[s].ls = numBlocks[lSs];
    if (candidate[rSs] == cNEVER)
      res->ss[s].rs = 1;
    else
      res->ss[s].rs = numBlocks[rSs];
    
    res->ss[s].behaviour = 
      (bdd_handle *) mem_alloc(sizeof(bdd_handle)*res->ss[s].ls*res->ss[s].rs);
    
    if (candidate[s] == cNEVER) {

      /* make trivial transition functions for untouched state spaces */
      res->ss[s].bddm = bdd_new_manager(1, 0); 
      bdd_handle_find_leaf_hashed_add_root(res->ss[s].bddm, 0);
      /* handle 0 contains root of BDD leaf 0 */
      for (i = 0; i < res->ss[s].ls; i++)
	for (j = 0; j < res->ss[s].rs; j++)
	  BEH(res->ss[s], i, j) = 0;
    }
    else { 
      int org_size = mona_bdd_size(orig->ss[s].bddm);
      int new_size = mona_bdd_size(res->ss[s].bddm);
      int new_size_after;

      /* make final transition functions for the processed state spaces */
      bdd_prepare_apply1(orig->ss[s].bddm);
      for (i = 0; i < orig->ss[lSs].size; i++)
	for (j = 0; j < orig->ss[rSs].size; j++) {
	  bdd_apply1(orig->ss[s].bddm, 
		     BDD_ROOT(orig->ss[s].bddm, BEH(orig->ss[s], i, j)),
		     res->ss[s].bddm,
		     fn_block);
	  BEH(res->ss[s], block[lSs][i], block[rSs][j]) =  
	    BDD_LAST_HANDLE(res->ss[s].bddm);
	}

      new_size_after = mona_bdd_size(res->ss[s].bddm);
      invariant(org_size >= new_size);
      invariant(new_size_after == new_size);
    }
    
    /* set size and initial */
    res->ss[s].size = numBlocks[s];
    res->ss[s].initial = block[s][orig->ss[s].initial];
  }

  /* set final status */
  res->final = (int *) mem_alloc(sizeof(int)*res->ss[0].size);
  for (i = 0; i < orig->ss[0].size; i++)
    res->final[block[0][i]] = orig->final[i];

  /* clean up */
  for (s = 0; s < guide.numSs; s++)
    mem_free(block[s]);
  mem_free(block);
  mem_free(transposed);
  mem_free(numBlocks);
  mem_free(candidate);
  mem_free(sorted);
  mem_free(original);
  mem_free(qb);
  mem_free(matrix);

  return res;
}
