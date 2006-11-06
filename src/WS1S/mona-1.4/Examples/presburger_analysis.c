/* presburger_analysis.c - DFA package example application */

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

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "dfa.h"

int decode_example(char *example, int row, int num_rows)
{
  /* decode one row of the example string */
  int i, val = 0, length = strlen(example)/(num_rows+1);
  for (i = length-1; i > 0; i--)
    val = val<<1 | (example[row*length+i] == '1');
  return val;
}

int main(int argc, char *argv[])
{
  char **vars;
  int *orders;
  DFA *a;
  char *example;
  unsigned indices[1];
  unsigned index;

  if (argc != 3) {
    printf("usage: %s <dfa-file> <variable-name>\n", argv[0]);
    exit(-1);
  }

  /* initialize the BDD package */
  mona_bdd_init();

  /* import the automaton */
  a = dfaImport(argv[1], &vars, &orders);
  if (!a) {
    printf("error: unable to import '%s'\n", argv[1]);
    exit(-1);
  }

  /* find the index */
  for (index = 0; vars[index]; index++)
    if (strcmp(vars[index], argv[2]) == 0)
      break;
  if (!vars[index]) {
    printf("error: '%s' not found in '%s'\n", argv[2], argv[1]);
    exit(-1);
  }

  /* 'dfaMakeExample' finds a string leading from the initial state
     to a nearest accepting state, 
     this string represents a binary encoded number for
     each free variable */
  indices[0] = index;
  example = dfaMakeExample(a, 1, 1, indices);
  
  /* print the result */
  if (!example)
    printf("relation is unsatisfiable!\n");
  else 
    printf("satisfying example:\n"
	   "%s = %d\n", 
	   argv[2], decode_example(example, 0, 1));

  /* clean up */
  dfaFree(a);

  return 0;
}
