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
#include "dlmalloc.h"
#include <stdio.h>
#include <string.h>

int memlimit = 0;

void mem_error()
{
  if (memlimit)
    printf("\n\n-----\n"
           "Interactive Demo memory limit exceeded, execution stopped.\n");
  else
    printf("\n*** out of memory, execution aborted ***\n");
  exit(-1);
}

void *mem_alloc(size_t s)
{
  void *x = dlmalloc(s);
  if (!x)
    mem_error();
  return x;
}

void mem_free(void *x)
{
  dlfree(x);
}

void *mem_resize(void *x, size_t s)
{
  void *y = dlrealloc(x, s);
  if (!y)
    mem_error();
  return y;
}

void mem_copy(void *dst, void *src, size_t s)
{
  memcpy(dst, src, s);
}

void mem_zero(void *x, size_t s)
{
  memset(x, 0, s);
}

int mem_allocated()
{
  struct mallinfo m = dlmallinfo();
  return m.uordblks+m.hblkhd;
}
