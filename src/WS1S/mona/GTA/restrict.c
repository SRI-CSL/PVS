/* restrict.c */

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
#include "gta.h"

void gtaRestrict(GTA *g)
{
  int i;
  for (i = 0; i < g->ss[0].size; i++)
    if (g->final[i] == -1)
      g->final[i] = 0; /* turn rejects into don't-cares */
}

void gtaUnrestrict(GTA *g)
{
  int i;
  for (i = 0; i < g->ss[0].size; i++)
    if (g->final[i] == 0)
      g->final[i] = -1; /* turn don't-cares into rejects */
}

